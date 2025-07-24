/*
 *
 * Llama.prg - Harbour wrappers for llama.cpp,
 * which allows chatting with local AI models
 *
 * This work is based on llama.cpp project
 * https://github.com/ggerganov/llama.cpp
 *
 * Copyright 2025 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
*/

#include "arg.h"
#include "common.h"
#include "llama.h"
#include "sampling.h"

#include <cmath>
#include <cstdio>
#include <cstdarg>
#include <string>
#include <cstring>
#include <vector>

#if defined(__linux__) || defined(__unix__)
   #include <unistd.h>
#else
   #include <io.h>
#endif

common_params params;
llama_model * model = nullptr;
llama_context * ctx = nullptr;
const llama_vocab * vocab;
//llama_sampler * smpl = nullptr;
common_sampler * smpl = nullptr;
//llama_model_params model_params;
common_init_result llama_init;

std::vector<llama_token> embd_inp;
std::vector<llama_token> embd;

int n_remain = 0;
int n_past = 0;
int n_consumed = 0;
int n_ctx = 0;

int n_len = 256;
int n_cur;

extern "C" {

void llm_writelog( const char * sFile, const char * sTraceMsg, ... );
int llm_open_model( int argc, char **argv );
int llm_create_context( void );
int llm_ask_0( const char * szPrompt );
const char * llm_getnexttoken_0( void );
int llm_ask( const char * szPrompt );
const char * llm_getnexttoken( void );
void llm_close_model( void );
void llm_close_context( void );
void llm_print_timings( void );

void llm_writelog( const char * sFile, const char * sTraceMsg, ... ) {
   FILE *hFile;

   if( sFile == NULL )
   {
      hFile = fopen( "ac.log", "a" );
   }
   else
   {
      hFile = fopen( sFile, "a" );
   }

   if( hFile )
   {
      va_list ap;

      va_start( ap, sTraceMsg );
      vfprintf( hFile, sTraceMsg, ap );
      va_end( ap );

      fclose( hFile );
   }
}

int llm_open_model( int argc, char **argv ) {

   if( argc < 2 )
      return 1;
/*
   model_params = llama_model_default_params();
   ggml_backend_load_all();
   model = llama_model_load_from_file( argv[2], model_params );
*/
/*
   int i;
   llm_writelog( NULL, "argc: %d\n", argc );
   for( i = 1; i < argc; i ++ );
      llm_writelog( NULL, "%s\n", argv[i] );
   llm_writelog( NULL, "====\n" );
*/
   if( !common_params_parse( argc, argv, params, LLAMA_EXAMPLE_MAIN, NULL ) )
      return 2;

   common_init();

   llama_backend_init();
   llama_numa_init( params.numa );

   model = nullptr;
   ctx = nullptr;
   smpl = nullptr;

/*
   llama_init = common_init_from_params( params );
   model = llama_init.model.get();
*/

   auto mparams = common_model_params_to_llama(params);
   model = llama_model_load_from_file( params.model.path.c_str(), mparams );

   if( model == NULL ) {
       return 3;
   }
   vocab = llama_model_get_vocab( model );

   // Should not run without any tokens
   embd_inp.clear();
   const bool add_bos = llama_vocab_get_add_bos( vocab ) && !params.use_jinja;
   if( add_bos ) {
       embd_inp.push_back(llama_vocab_bos(vocab));
   } else {
       llama_model_free( model );
       model = nullptr;
       return 4;
   }

   // number of tokens to keep when resetting context
   if (params.n_keep < 0 || params.n_keep > (int) embd_inp.size()) {
       params.n_keep = (int)embd_inp.size();
   } else {
       params.n_keep += add_bos; // always keep the BOS token
   }

   return 0;
}

int llm_create_context( void ) {

   auto cparams = common_context_params_to_llama( params );
   ctx = llama_init_from_model( model, cparams );

   if( ctx == NULL ) {
       return 1;
   }

   // initialize the sampler

   auto & sparams = params.sampling;
   smpl = common_sampler_init( model, sparams );
   if( !smpl ) {
      return 1;
   }

   if (llama_model_has_encoder(model)) {
      int enc_input_size = embd_inp.size();
      llama_token * enc_input_buf = embd_inp.data();

      if (llama_encode(ctx, llama_batch_get_one(enc_input_buf, enc_input_size))) {
          return 2;
      }

      llama_token decoder_start_token_id = llama_model_decoder_start_token(model);
      if (decoder_start_token_id == LLAMA_TOKEN_NULL) {
          decoder_start_token_id = llama_vocab_bos(vocab);
      }

      embd_inp.clear();
      embd_inp.push_back(decoder_start_token_id);
   }

   n_remain = params.n_predict;
   n_past = 0;
   n_consumed = 0;
   n_ctx = llama_n_ctx(ctx);

   return 0;
}

int llm_ask_0( const char * szPrompt ) {
   common_sampler_reset( smpl );
   const int n_prompt = -llama_tokenize( vocab, szPrompt, strlen(szPrompt), NULL, 0, true, true );

   // allocate space for the tokens and tokenize the prompt
   std::vector<llama_token> prompt_tokens( n_prompt );
   if (llama_tokenize( vocab, szPrompt, strlen( szPrompt ), prompt_tokens.data(),
      prompt_tokens.size(), true, true) < 0) {
      return 1;
   }

   llama_batch batch = llama_batch_get_one( prompt_tokens.data(), prompt_tokens.size() );

   if( llama_decode( ctx, batch ) ) {
       return 2;
   }

   n_cur = batch.n_tokens;
   return 0;
}

const char * llm_getnexttoken_0( void ) {
   char buf[128];
   int n = 0;

   if( n_cur < n_len ) {
      //llama_token new_token_id = llama_sampler_sample( smpl, ctx, -1 );
      llama_token new_token_id = (llama_token) common_sampler_sample(smpl, ctx, -1);
      // is it an end of generation?
      if (llama_vocab_is_eog(vocab, new_token_id)) {
         return NULL;
      }

      n = llama_token_to_piece(vocab, new_token_id, buf, sizeof(buf), 0, true);
      if (n <= 0) {
          //fprintf(stderr, "%s: error: failed to convert token to piece\n", __func__);
          return NULL;
      }

      // prepare the next batch with the sampled token
      llama_batch batch = llama_batch_get_one( &new_token_id, 1 );
      n_cur += batch.n_tokens;
      if (llama_decode(ctx, batch)) {
          //fprintf(stderr, "%s : failed to eval, return code %d\n", __func__, 1);
          return NULL;
      }
   } else
      return NULL;

   std::string s( buf, n );
   return s.c_str();

}

int llm_ask( const char * szPrompt ) {

   common_sampler_reset( smpl );

   std::string buffer( szPrompt );
   if( buffer.length() > 1 ) {
      if (params.escape)
          string_process_escapes(buffer);

      std::string user_inp = std::move( buffer );
      const auto line_inp = common_tokenize( ctx, user_inp, false, false );

      // if user stop generation mid-way, we must add EOT to finish model's last response
      embd_inp.insert( embd_inp.end(), line_inp.begin(), line_inp.end() );
      n_remain -= line_inp.size();
   }
   return 0;
}

const char * llm_getnexttoken( void ) {

   bool bOut = false;

   if( !embd.empty() ) {
      // Note: (n_ctx - 4) here is to match the logic for commandline prompt handling via
      // --prompt or --file which uses the same value.
      int max_embd_size = n_ctx - 4;

      // Ensure the input doesn't exceed the context size by truncating embd if necessary.
      if ((int) embd.size() > max_embd_size) {
          const int skipped_tokens = (int) embd.size() - max_embd_size;
          embd.resize(max_embd_size);
      }

      // infinite text generation via context shifting
      // if we run out of context:
      // - take the n_keep first tokens from the original prompt (via n_past)
      // - take half of the last (n_ctx - n_keep) tokens and recompute the logits in batches
      if (n_past + (int) embd.size() >= n_ctx) {
         if (!params.ctx_shift){
             //LOG_INF("\n\n%s: context full and context shift is disabled => stopping\n", __func__);
             return NULL;
         }

         if (params.n_predict == -2) {
             //LOG_INF("\n\n%s: context full and n_predict == -%d => stopping\n", __func__, params.n_predict);
             return NULL;
         }

         const int n_left    = n_past - params.n_keep;
         const int n_discard = n_left/2;

         llama_kv_self_seq_rm (ctx, 0, params.n_keep, params.n_keep + n_discard);
         llama_kv_self_seq_add(ctx, 0, params.n_keep + n_discard, n_past, -n_discard);

         n_past -= n_discard;
      }

      for (int i = 0; i < (int) embd.size(); i += params.n_batch) {
         int n_eval = (int) embd.size() - i;
         if (n_eval > params.n_batch) {
             n_eval = params.n_batch;
         }

         if (llama_decode(ctx, llama_batch_get_one(&embd[i], n_eval))) {
             return NULL;
         }

         n_past += n_eval;
      }
   }

   embd.clear();

   if( (int) embd_inp.size() <= n_consumed ) {
     //LOG_INF("--- M31 ---\n");
      const llama_token id = common_sampler_sample(smpl, ctx, -1);
      common_sampler_accept(smpl, id, /* accept_grammar= */ true);
      embd.push_back(id);

      bOut = true;
      --n_remain;         // decrement remaining sampling budget
   } else {
      // some user input remains from prompt or interaction, forward it to processing
      while ((int) embd_inp.size() > n_consumed) {
         embd.push_back(embd_inp[n_consumed]);

         // push the prompt in the sampling context in order to apply repetition penalties later
         // for the prompt, we don't apply grammar rules
         common_sampler_accept(smpl, embd_inp[n_consumed], /* accept_grammar= */ false);

         ++n_consumed;
         if ((int) embd.size() >= params.n_batch) {
            return NULL;
         }
      }
   }

   // display text
   std::string sRes;
   if( bOut ) {
      for (auto id : embd) {
         sRes += common_token_to_piece(ctx, id, params.special);
      }
   }

   // if not currently processing queued inputs;
   if ((int) embd_inp.size() <= n_consumed) {
      // deal with end of generation tokens in interactive mode
      if( llama_vocab_is_eog(vocab, common_sampler_last(smpl)) ) {
         return NULL;
      }
   }

   // Respect the maximum number of tokens and drop back to user input when reached.
   // We skip this logic when n_predict == -1 (infinite) or -2 (stop at context size).
   if( n_remain <= 0 && params.n_predict >= 0 ) {
      n_remain = params.n_predict;
      return NULL;
   }

   return sRes.c_str();
}

void llm_close_context( void ) {

   if( smpl ) {
      common_sampler_free( smpl );
      //llama_sampler_free( smpl );
      smpl = nullptr;
   }
   if( ctx ) {
      llama_free( ctx );
      ctx = nullptr;
   }
}

void llm_close_model( void ) {
   if( model ) {
      llama_model_free( model );
      model = nullptr;
   }
   llama_backend_free();
}

void llm_print_timings( void ) {

   if( smpl && ctx )
      common_perf_print( ctx, smpl );
}

}