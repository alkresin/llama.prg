/*
 *
 * Llama.prg - Harbour wrappers for llama.cpp,
 * which allows chatting with local AI models
 *
 * hwisper.cpp - wrappers for Whisper
 *
 * This work is based on llama.cpp and whisper projects
 * https://github.com/ggerganov/llama.cpp
 * https://github.com/ggerganov/whisper.cpp
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
*/

#include "common.h"

#include "whisper.h"
#include "common-whisper.h"
#include "grammar-parser.h"

#include <cmath>
#include <fstream>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <regex>
#include <string>
#include <thread>
#include <vector>
#include <cstring>
#include <cfloat>

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "item.api"

#if defined(_MSC_VER)
#pragma warning(disable: 4244 4267) // possible loss of data
#endif

void wh_writelog( const char * sFile, const char * sTraceMsg, ... ) {
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

// command-line parameters
struct whisper_params {
    int32_t n_threads     = std::min(4, (int32_t) std::thread::hardware_concurrency());
    int32_t n_processors  = 1;
    int32_t offset_t_ms   = 0;
    int32_t offset_n      = 0;
    int32_t duration_ms   = 0;
    int32_t progress_step = 5;
    int32_t max_context   = -1;
    int32_t max_len       = 0;
    int32_t best_of       = whisper_full_default_params(WHISPER_SAMPLING_GREEDY).greedy.best_of;
    int32_t beam_size     = whisper_full_default_params(WHISPER_SAMPLING_BEAM_SEARCH).beam_search.beam_size;
    int32_t audio_ctx     = 0;

    float word_thold      =  0.01f;
    float entropy_thold   =  2.40f;
    float logprob_thold   = -1.00f;
    float no_speech_thold =  0.6f;
    float grammar_penalty = 100.0f;
    float temperature     = 0.0f;
    float temperature_inc = 0.2f;

    bool debug_mode      = false;
    bool translate       = false;
    bool detect_language = false;
    bool diarize         = false;
    bool tinydiarize     = false;
    bool split_on_word   = false;
    bool no_fallback     = false;
    bool output_txt      = false;
    bool output_vtt      = false;
    bool output_srt      = false;
    bool output_wts      = false;
    bool output_csv      = false;
    bool output_jsn      = false;
    bool output_jsn_full = false;
    bool output_lrc      = false;
    bool no_prints       = false;
    bool print_special   = false;
    bool print_colors    = false;
    bool print_confidence= false;
    bool print_progress  = false;
    bool no_timestamps   = false;
    bool log_score       = false;
    bool use_gpu         = true;
    bool flash_attn      = false;
    bool suppress_nst    = false;

    std::string language  = "en";
    std::string prompt;
    std::string font_path = "/System/Library/Fonts/Supplemental/Courier New Bold.ttf";
    std::string model     = "models/ggml-base.en.bin";
    std::string grammar;
    std::string grammar_rule;

    // [TDRZ] speaker turn string
    std::string tdrz_speaker_turn = " [SPEAKER_TURN]"; // TODO: set from command line

    // A regular expression that matches tokens to suppress
    std::string suppress_regex;

    std::string openvino_encode_device = "CPU";

    std::string dtw = "";

    std::vector<std::string> fname_inp = {};
    std::vector<std::string> fname_out = {};

    grammar_parser::parse_state grammar_parsed;

    // Voice Activity Detection (VAD) parameters
    bool        vad           = false;
    std::string vad_model     = "";
    float       vad_threshold = 0.5f;
    int         vad_min_speech_duration_ms = 250;
    int         vad_min_silence_duration_ms = 100;
    float       vad_max_speech_duration_s = FLT_MAX;
    int         vad_speech_pad_ms = 30;
    float       vad_samples_overlap = 0.1f;
};

static whisper_params params;
static struct whisper_context * ctx;
static bool isAborted;

static PHB_DYNS s_pSymTest = NULL;

char* whisper_param_turn_lowercase(char* in){
   int string_len = strlen(in);
   for(int i = 0; i < string_len; i++){
       *(in+i) = tolower((unsigned char)*(in+i));
   }
   return in;
}

bool whisper_params_parse( char * szParams ) {

   int iPos = 0, iPos1, iLen = strlen( szParams ), iArgLen;
   char szArg[256], szArg1[20], szArg2[220];

   while( iPos < iLen ) {

      iPos1 = iPos;
      while( iPos < iLen && szParams[iPos] != '~' ) iPos ++;
      iArgLen = iPos - iPos1;
      memcpy( szArg, szParams + iPos1, iArgLen );
      szArg[iArgLen] = '\0';
      iPos ++;

      iPos1 = 0;
      while( iPos1 < iArgLen && szArg[iPos1] != '=' ) iPos1 ++;
      memcpy( szArg1, szArg, iPos1 );
      szArg1[iPos1] = '\0';
      memcpy( szArg2, szArg + iPos1 + 1, iArgLen - iPos1 );
      szArg2[iArgLen - iPos1] = '\0';

      std::string arg = szArg1;

      if( arg == "t" )     { params.n_threads       = std::stoi( szArg2 ); }
      else if( arg == "p" )    { params.n_processors    = std::stoi( szArg2 ); }
      else if( arg == "ot" )   { params.offset_t_ms     = std::stoi( szArg2 ); }
      else if( arg == "on" )   { params.offset_n        = std::stoi( szArg2 ); }
      else if( arg == "d"  )   { params.duration_ms     = std::stoi( szArg2 ); }
      else if( arg == "mc" )   { params.max_context     = std::stoi( szArg2 ); }
      else if( arg == "ml" )   { params.max_len         = std::stoi( szArg2 ); }
      else if( arg == "bo" )   { params.best_of         = std::stoi( szArg2 ); }
      else if( arg == "bs" )   { params.beam_size       = std::stoi( szArg2 ); }
      else if( arg == "ac" )   { params.audio_ctx       = std::stoi( szArg2 ); }
      else if( arg == "wt" )   { params.word_thold      = std::stof( szArg2 ); }
      else if( arg == "et" )   { params.entropy_thold   = std::stof( szArg2 ); }
      else if( arg == "lpt" )  { params.logprob_thold   = std::stof( szArg2 ); }
      else if( arg == "debug" ){ params.debug_mode      = true; }
      else if( arg == "tr" )   { params.translate       = true; }
      else if( arg == "di" )   { params.diarize         = true; }
      else if( arg == "tdrz" ) { params.tinydiarize     = true; }
      else if( arg == "sow" )  { params.split_on_word   = true; }
      else if( arg == "nf" )   { params.no_fallback     = true; }
      else if( arg == "otxt" ) { params.output_txt      = true; }
      else if (arg == "osrt" ) { params.output_srt      = true; }
      else if (arg == "olrc" ) { params.output_lrc      = true; }
      else if (arg == "oj" )   { params.output_jsn      = true; }
      else if( arg == "of" )   { params.fname_out.emplace_back( szArg2 ); }
      else if( arg == "np" )   { params.no_prints       = true; }
      else if( arg == "ps" )   { params.print_special   = true; }
      else if( arg == "pp" )   { params.print_progress  = true; }
      else if( arg == "nt" )   { params.no_timestamps   = true; }
      else if( arg == "l"  )   { params.language        = whisper_param_turn_lowercase( szArg2 ); }
      else if( arg == "dl" )   { params.detect_language = true; }
      else if( arg == "prompt" ) { params.prompt       =  szArg2 ; }
      else if( arg == "oved" ) { params.openvino_encode_device = szArg2 ; }
      else if( arg == "ls" )   { params.log_score       = true; }
      else if( arg == "ng" )   { params.use_gpu         = false; }
      else if( arg == "suppress-regex" )  { params.suppress_regex  = szArg2 ; }
      else if( arg == "grammar")         { params.grammar         = szArg2 ; }
      else if( arg == "grammar-rule")    { params.grammar_rule    = szArg2 ; }
      else if( arg == "grammar-penalty") { params.grammar_penalty = std::stof( szArg2 ); }
      else {
         return 0;
      }
   }

   return true;
}

struct whisper_print_user_data {
   const whisper_params * params;

   const std::vector<std::vector<float>> * pcmf32s;
   int progress_prev;
};

std::string estimate_diarization_speaker(std::vector<std::vector<float>> pcmf32s,
  int64_t t0, int64_t t1, bool id_only = false) {

   std::string speaker = "";
   const int64_t n_samples = pcmf32s[0].size();

   const int64_t is0 = timestamp_to_sample(t0, n_samples, WHISPER_SAMPLE_RATE);
   const int64_t is1 = timestamp_to_sample(t1, n_samples, WHISPER_SAMPLE_RATE);

   double energy0 = 0.0f;
   double energy1 = 0.0f;

   for (int64_t j = is0; j < is1; j++) {
       energy0 += fabs(pcmf32s[0][j]);
       energy1 += fabs(pcmf32s[1][j]);
   }

   if (energy0 > 1.1*energy1) {
       speaker = "0";
   } else if (energy1 > 1.1*energy0) {
       speaker = "1";
   } else {
       speaker = "?";
   }

   if (!id_only) {
       speaker.insert(0, "(speaker ");
       speaker.append(")");
   }

   return speaker;
}

/*
void whisper_print_progress_callback( struct whisper_context *, struct whisper_state *,
   int progress, void * user_data) {

   int progress_step = ((whisper_print_user_data *) user_data)->params->progress_step;
   int * progress_prev  = &(((whisper_print_user_data *) user_data)->progress_prev);
   if (progress >= *progress_prev + progress_step) {
       *progress_prev += progress_step;
       fprintf(stderr, "%s: progress = %3d%%\n", __func__, progress);
   }
}
*/

#define SEGMENT_BUF_LEN  4096

void whisper_print_segment_callback( struct whisper_context * ctx, struct whisper_state *,
   int n_new, void * user_data ) {

   char buff[SEGMENT_BUF_LEN], *ptr;
   const auto & params  = *((whisper_print_user_data *) user_data)->params;
   const auto & pcmf32s = *((whisper_print_user_data *) user_data)->pcmf32s;
   const int n_segments = whisper_full_n_segments( ctx );

   std::string speaker = "";

   int64_t t0 = 0;
   int64_t t1 = 0;

   // print the last n_new segments
   const int s0 = n_segments - n_new;

   ptr = buff;

   if (s0 == 0) {
      ptr += snprintf( ptr, SEGMENT_BUF_LEN-(ptr-buff), "\n" );
   }

   for( int i = s0; i < n_segments; i++ ) {

      if (!params.no_timestamps || params.diarize) {
         t0 = whisper_full_get_segment_t0(ctx, i);
         t1 = whisper_full_get_segment_t1(ctx, i);
      }

      if (!params.no_timestamps) {
         ptr += snprintf( ptr, SEGMENT_BUF_LEN-(ptr-buff), "[%s --> %s]  ", to_timestamp(t0).c_str(), to_timestamp(t1).c_str());
      }

      if (params.diarize && pcmf32s.size() == 2) {
         speaker = estimate_diarization_speaker(pcmf32s, t0, t1);
      }

      {
         const char * text = whisper_full_get_segment_text(ctx, i);
         ptr += snprintf( ptr, SEGMENT_BUF_LEN-(ptr-buff), "%s%s", speaker.c_str(), text );
      }

      if (params.tinydiarize) {
         if (whisper_full_get_segment_speaker_turn_next(ctx, i)) {
             ptr += snprintf( ptr, SEGMENT_BUF_LEN-(ptr-buff), "%s", params.tdrz_speaker_turn.c_str() );
         }
      }

      // with timestamps or speakers: each segment on new line
      if (!params.no_timestamps || params.diarize) {
         ptr += snprintf( ptr, SEGMENT_BUF_LEN-(ptr-buff), "\n" );
      }

      if( s_pSymTest && hb_dynsymIsFunction( s_pSymTest ) ) {

         hb_vmPushDynSym( s_pSymTest );
         hb_vmPushNil();
         hb_vmPushString( buff, ptr - buff );
         hb_vmDo( 1 );

      } else {
         printf( buff );
         fflush( stdout );
      }
      ptr = buff;
   }

}

char *escape_double_quotes_and_backslashes(const char *str) {
    if (str == NULL) {
        return NULL;
    }

    size_t escaped_length = strlen(str) + 1;

    for (size_t i = 0; str[i] != '\0'; i++) {
        if (str[i] == '"' || str[i] == '\\') {
            escaped_length++;
        }
    }

    char *escaped = (char *)calloc(escaped_length, 1); // pre-zeroed
    if (escaped == NULL) {
        return NULL;
    }

    size_t pos = 0;
    for (size_t i = 0; str[i] != '\0'; i++) {
        if (str[i] == '"' || str[i] == '\\') {
            escaped[pos++] = '\\';
        }
        escaped[pos++] = str[i];
    }

    // no need to set zero due to calloc() being used prior

    return escaped;
}

static void output_txt(struct whisper_context * ctx, std::ofstream & fout, const whisper_params & params, std::vector<std::vector<float>> pcmf32s) {
    const int n_segments = whisper_full_n_segments(ctx);
    for (int i = 0; i < n_segments; ++i) {
        const char * text = whisper_full_get_segment_text(ctx, i);
        std::string speaker = "";

        if (params.diarize && pcmf32s.size() == 2)
        {
            const int64_t t0 = whisper_full_get_segment_t0(ctx, i);
            const int64_t t1 = whisper_full_get_segment_t1(ctx, i);
            speaker = estimate_diarization_speaker(pcmf32s, t0, t1);
        }

        fout << speaker << text << "\n";
    }
}

bool output_string( const whisper_params & params,
   std::vector<std::vector<float>> pcmf32s, std::string * s ) {

   std::stringstream fout;
   const int n_segments = whisper_full_n_segments(ctx);
   for (int i = 0; i < n_segments; ++i) {
      const char * text = whisper_full_get_segment_text(ctx, i);
      std::string speaker = "";

      if (params.diarize && pcmf32s.size() == 2)
      {
         const int64_t t0 = whisper_full_get_segment_t0(ctx, i);
         const int64_t t1 = whisper_full_get_segment_t1(ctx, i);
         speaker = estimate_diarization_speaker(pcmf32s, t0, t1);
      }

      fout << speaker << text << "\n";
   }
   *s = fout.str();

   return true;
}

static void output_srt(struct whisper_context * ctx, std::ofstream & fout, const whisper_params & params, std::vector<std::vector<float>> pcmf32s) {
    const int n_segments = whisper_full_n_segments(ctx);
    for (int i = 0; i < n_segments; ++i) {
        const char * text = whisper_full_get_segment_text(ctx, i);
        const int64_t t0 = whisper_full_get_segment_t0(ctx, i);
        const int64_t t1 = whisper_full_get_segment_t1(ctx, i);
        std::string speaker = "";

        if (params.diarize && pcmf32s.size() == 2)
        {
            speaker = estimate_diarization_speaker(pcmf32s, t0, t1);
        }

        fout << i + 1 + params.offset_n << "\n";
        fout << to_timestamp(t0, true) << " --> " << to_timestamp(t1, true) << "\n";
        fout << speaker << text << "\n\n";
    }
}

static void output_lrc(struct whisper_context * ctx, std::ofstream & fout, const whisper_params & params, std::vector<std::vector<float>> pcmf32s) {
    fout << "[by:whisper.cpp]\n";

    const int n_segments = whisper_full_n_segments(ctx);
    for (int i = 0; i < n_segments; ++i) {
        const char * text = whisper_full_get_segment_text(ctx, i);
        const int64_t t = whisper_full_get_segment_t0(ctx, i);

        int64_t msec = t * 10;
        int64_t min = msec / (1000 * 60);
        msec = msec - min * (1000 * 60);
        int64_t sec = msec / 1000;
        msec = msec - sec * 1000;

        char buf[16];
        snprintf(buf, sizeof(buf), "%02d:%02d.%02d", (int) min, (int) sec, (int) ( msec / 10));
        std::string timestamp_lrc = std::string(buf);
        std::string speaker = "";

        if (params.diarize && pcmf32s.size() == 2)
        {
            const int64_t t0 = whisper_full_get_segment_t0(ctx, i);
            const int64_t t1 = whisper_full_get_segment_t1(ctx, i);
            speaker = estimate_diarization_speaker(pcmf32s, t0, t1);
        }

        fout <<  '[' << timestamp_lrc << ']' << speaker << text << "\n";
    }
}

static void output_score(struct whisper_context * ctx, std::ofstream & fout, const whisper_params & /*params*/, std::vector<std::vector<float>> /*pcmf32s*/) {
    const int n_segments = whisper_full_n_segments(ctx);
    // fprintf(stderr,"segments: %d\n",n_segments);
    for (int i = 0; i < n_segments; ++i) {
        const int n_tokens = whisper_full_n_tokens(ctx, i);
        // fprintf(stderr,"tokens: %d\n",n_tokens);
        for (int j = 0; j < n_tokens; j++) {
            auto token = whisper_full_get_token_text(ctx, i, j);
            auto probability = whisper_full_get_token_p(ctx, i, j);
            fout << token << '\t' << probability << std::endl;
            // fprintf(stderr,"token: %s %f\n",token,probability);
	    }
    }
}

static void output_json(
             struct whisper_context * ctx,
                      std::ofstream & fout,
               const whisper_params & params,
    std::vector<std::vector<float>>   pcmf32s) {
    const bool full = params.output_jsn_full;
    int indent = 0;

    auto doindent = [&]() {
        for (int i = 0; i < indent; i++) fout << "\t";
    };

    auto start_arr = [&](const char *name) {
        doindent();
        fout << "\"" << name << "\": [\n";
        indent++;
    };

    auto end_arr = [&](bool end) {
        indent--;
        doindent();
        fout << (end ? "]\n" : "],\n");
    };

    auto start_obj = [&](const char *name) {
        doindent();
        if (name) {
            fout << "\"" << name << "\": {\n";
        } else {
            fout << "{\n";
        }
        indent++;
    };

    auto end_obj = [&](bool end) {
        indent--;
        doindent();
        fout << (end ? "}\n" : "},\n");
    };

    auto start_value = [&](const char *name) {
        doindent();
        fout << "\"" << name << "\": ";
    };

    auto value_s = [&](const char *name, const char *val, bool end) {
        start_value(name);
        char * val_escaped = escape_double_quotes_and_backslashes(val);
        fout << "\"" << val_escaped << (end ? "\"\n" : "\",\n");
        free(val_escaped);
    };

    auto end_value = [&](bool end) {
        fout << (end ? "\n" : ",\n");
    };

    auto value_i = [&](const char *name, const int64_t val, bool end) {
        start_value(name);
        fout << val;
        end_value(end);
    };

    auto value_f = [&](const char *name, const float val, bool end) {
        start_value(name);
        fout << val;
        end_value(end);
    };

    auto value_b = [&](const char *name, const bool val, bool end) {
        start_value(name);
        fout << (val ? "true" : "false");
        end_value(end);
    };

    auto times_o = [&](int64_t t0, int64_t t1, bool end) {
        start_obj("timestamps");
        value_s("from", to_timestamp(t0, true).c_str(), false);
        value_s("to", to_timestamp(t1, true).c_str(), true);
        end_obj(false);
        start_obj("offsets");
        value_i("from", t0 * 10, false);
        value_i("to", t1 * 10, true);
        end_obj(end);
    };

    start_obj(nullptr);
        value_s("systeminfo", whisper_print_system_info(), false);
        start_obj("model");
            value_s("type", whisper_model_type_readable(ctx), false);
            value_b("multilingual", whisper_is_multilingual(ctx), false);
            value_i("vocab", whisper_model_n_vocab(ctx), false);
            start_obj("audio");
                value_i("ctx", whisper_model_n_audio_ctx(ctx), false);
                value_i("state", whisper_model_n_audio_state(ctx), false);
                value_i("head", whisper_model_n_audio_head(ctx), false);
                value_i("layer", whisper_model_n_audio_layer(ctx), true);
            end_obj(false);
            start_obj("text");
                value_i("ctx", whisper_model_n_text_ctx(ctx), false);
                value_i("state", whisper_model_n_text_state(ctx), false);
                value_i("head", whisper_model_n_text_head(ctx), false);
                value_i("layer", whisper_model_n_text_layer(ctx), true);
            end_obj(false);
            value_i("mels", whisper_model_n_mels(ctx), false);
            value_i("ftype", whisper_model_ftype(ctx), true);
        end_obj(false);
        start_obj("params");
            value_s("model", params.model.c_str(), false);
            value_s("language", params.language.c_str(), false);
            value_b("translate", params.translate, true);
        end_obj(false);
        start_obj("result");
            value_s("language", whisper_lang_str(whisper_full_lang_id(ctx)), true);
        end_obj(false);
        start_arr("transcription");

            const int n_segments = whisper_full_n_segments(ctx);
            for (int i = 0; i < n_segments; ++i) {
                const char * text = whisper_full_get_segment_text(ctx, i);

                const int64_t t0 = whisper_full_get_segment_t0(ctx, i);
                const int64_t t1 = whisper_full_get_segment_t1(ctx, i);

                start_obj(nullptr);
                    times_o(t0, t1, false);
                    value_s("text", text, !params.diarize && !params.tinydiarize && !full);

                    if (full) {
                        start_arr("tokens");
                        const int n = whisper_full_n_tokens(ctx, i);
                        for (int j = 0; j < n; ++j) {
                            auto token = whisper_full_get_token_data(ctx, i, j);
                            start_obj(nullptr);
                                value_s("text", whisper_token_to_str(ctx, token.id), false);
                                if(token.t0 > -1 && token.t1 > -1) {
                                    // If we have per-token timestamps, write them out
                                    times_o(token.t0, token.t1, false);
                                }
                                value_i("id", token.id, false);
                                value_f("p", token.p, false);
                                value_f("t_dtw", token.t_dtw, true);
                            end_obj(j == (n - 1));
                        }
                        end_arr(!params.diarize && !params.tinydiarize);
                    }

                    if (params.diarize && pcmf32s.size() == 2) {
                        value_s("speaker", estimate_diarization_speaker(pcmf32s, t0, t1, true).c_str(), true);
                    }

                    if (params.tinydiarize) {
                        value_b("speaker_turn_next", whisper_full_get_segment_speaker_turn_next(ctx, i), true);
                    }
                end_obj(i == (n_segments - 1));
            }

        end_arr(true);
    end_obj(true);
}

void cb_log_disable(enum ggml_log_level , const char * , void * ) { }

#define PBUF_LEN  16384

HB_FUNC( LLM_WHISPER_PRINT_USAGE )
{

   char buff[16384], *ptr;

   ptr = buff;
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  t N                  [%-7d] number of threads to use during computation\n", params.n_threads);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  p N                  [%-7d] number of processors to use during computation\n", params.n_processors);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  ot N                 [%-7d] time offset in milliseconds\n", params.offset_t_ms);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  on N                 [%-7d] segment index offset\n", params.offset_n);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  d  N                 [%-7d] duration of audio to process in milliseconds\n", params.duration_ms);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  mc N                 [%-7d] maximum number of text context tokens to store\n", params.max_context);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  ml N                 [%-7d] maximum segment length in characters\n", params.max_len);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  sow                  [%-7s] split on word rather than on token\n", params.split_on_word ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  bo N                 [%-7d] number of best candidates to keep\n", params.best_of);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  bs N                 [%-7d] beam size for beam search\n", params.beam_size);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  ac N                 [%-7d] audio context size (0 - all)\n", params.audio_ctx);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  wt N                 [%-7.2f] word timestamp probability threshold\n",params.word_thold);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  et N                 [%-7.2f] entropy threshold for decoder fail\n", params.entropy_thold);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  lpt N                [%-7.2f] log probability threshold for decoder fail\n", params.logprob_thold);
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  debug                [%-7s] enable debug mode (eg. dump log_mel)\n",params.debug_mode ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  tr                   [%-7s] translate from source language to english\n", params.translate ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  di                   [%-7s] stereo audio diarization\n", params.diarize ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  tdrz                 [%-7s] enable tinydiarize (requires a tdrz model)\n", params.tinydiarize ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  nf                   [%-7s] do not use temperature fallback while decoding\n", params.no_fallback ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  otxt                 [%-7s] output result in a text file\n", params.output_txt ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  osrt,                [%-7s] output result in a srt file\n", params.output_srt ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  olrc,                [%-7s] output result in a lrc file\n", params.output_lrc ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  oj,                  [%-7s] output result in a JSON file\n", params.output_jsn ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  of FNAME             [%-7s] output file path (without file extension)\n",      "");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  np                   [%-7s] do not print anything other than the results\n", params.no_prints ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  ps                   [%-7s] print special tokens\n", params.print_special ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  pp                   [%-7s] print progress\n", params.print_progress ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  nt                   [%-7s] do not print timestamps\n", params.no_timestamps ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  l LANG               [%-7s] spoken language ('auto' for auto-detect)\n", params.language.c_str());
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  dl                   [%-7s] exit after automatically detecting language\n",params.detect_language ? "true" : "false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  prompt PROMPT        [%-7s] initial prompt (max n_text_ctx/2 tokens)\n", params.prompt.c_str());
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  oved D               [%-7s] the OpenVINO device used for encode inference\n", params.openvino_encode_device.c_str());
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  ls,                  [%-7s] log best decoder scores of tokens\n", params.log_score?"true":"false");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  ng,                  [%-7s] disable GPU\n", params.use_gpu ? "false" : "true");
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  suppress-regex REGEX [%-7s] regular expression matching tokens to suppress\n", params.suppress_regex.c_str());
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  grammar GRAMMAR      [%-7s] GBNF grammar to guide decoding\n", params.grammar.c_str());
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  grammar-rule RULE    [%-7s] top-level GBNF grammar rule name\n", params.grammar_rule.c_str());
   ptr += snprintf( ptr, PBUF_LEN-(ptr-buff), "  grammar-penalty N    [%-7.1f] scales down logits of nongrammar tokens\n", params.grammar_penalty);

   hb_retc( buff );
}

HB_FUNC( LLM_WHISPER_SET_PARAMS )
{

   if( HB_ISCHAR(1) ) {
      if( whisper_params_parse( (char*) hb_parc(1) ) == false ) {
         hb_retni( 1 );
         return;
      }
   }
   if( params.language != "auto" && whisper_lang_id(params.language.c_str() ) == -1) {
      params.language = "auto";
      hb_retni( 2 );
      return;
   }

   if( params.diarize && params.tinydiarize )
      params.tinydiarize = 0;

   if (params.no_prints) {
      whisper_log_set(cb_log_disable, NULL);
   }

   hb_retni( 0 );
}

HB_FUNC( LLM_WHISPER_OPEN_MODEL )
{
   params.model = hb_parc( 1 );

   struct whisper_context_params  cparams = whisper_context_default_params();
   cparams.use_gpu = params.use_gpu;

   ctx = whisper_init_from_file_with_params(params.model.c_str(), cparams);

   if (ctx == nullptr) {
      fprintf(stderr, "error: failed to initialize whisper context\n");
      hb_retni( 3 );
      return;
   }

   // initialize openvino encoder. this has no effect on whisper.cpp builds that don't have OpenVINO configured
   whisper_ctx_init_openvino_encoder(ctx, nullptr, params.openvino_encode_device.c_str(), nullptr);

   // whisper init
   if (!params.grammar.empty()) {
      auto & grammar = params.grammar_parsed;
      if (is_file_exist(params.grammar.c_str())) {
         // read grammar from file
         std::ifstream ifs(params.grammar.c_str());
         const std::string txt = std::string((std::istreambuf_iterator<char>(ifs)), std::istreambuf_iterator<char>());
         grammar = grammar_parser::parse(txt.c_str());
      } else {
         // read grammar from string
         grammar = grammar_parser::parse(params.grammar.c_str());
      }

      // will be empty (default) if there are parse errors
      if (grammar.rules.empty()) {
         fprintf(stderr, "error: failed to parse grammar \"%s\"\n", params.grammar.c_str());
         hb_retni( 4 );
         return;
      } else {
         fprintf(stderr, "%s: grammar:\n", __func__);
         grammar_parser::print_grammar(stderr, grammar);
         fprintf(stderr, "\n");
      }
   }
   hb_retni( 0 );
}

HB_FUNC( LLM_WHISPER_CLOSE_MODEL )
{
   whisper_free( ctx );
}

/*
 * llm_whisper_recognize( cFile )
 */
HB_FUNC( LLM_WHISPER_RECOGNIZE )
{
   short int bToString = ( hb_pcount() > 1 && HB_ISBYREF( 2 ) );
   std::vector<float> pcmf32;               // mono-channel F32 PCM
   std::vector<std::vector<float>> pcmf32s; // stereo-channel F32 PCM
   //const auto & fname_inp = hb_parc( 1 );
   std::string fname_inp = hb_parc( 1 );

   struct fout_factory {
       std::string fname_out;
       const size_t basename_length;
       const bool is_stdout;
       bool used_stdout;
       decltype(whisper_print_segment_callback) * const print_segment_callback;
       std::ofstream fout;

       fout_factory (const std::string & fname_out_, const std::string & fname_inp, whisper_params & params) :
               fname_out{!fname_out_.empty() ? fname_out_ : fname_inp},
               basename_length{fname_out.size()},
               is_stdout{fname_out == "-"},
               used_stdout{},
               print_segment_callback{is_stdout ? nullptr : whisper_print_segment_callback} {
           if (!print_segment_callback) {
               params.print_progress = false;
           }
       }

       bool open(const char * ext, const char * function) {
           if (is_stdout) {
               if (used_stdout) {
                   fprintf(stderr, "warning: Not appending multiple file formats to stdout\n");
                   return false;
               }

               used_stdout = true;
#ifdef _WIN32
               fout = std::ofstream{"CON"};
#else
               fout = std::ofstream{"/dev/stdout"};
#endif
               // Not using fprintf stderr here because it might equal stdout
               // Also assuming /dev is mounted
               return true;
           }

           fname_out.resize(basename_length);
           fname_out += ext;
           fout = std::ofstream{fname_out};
           if (!fout.is_open()) {
               fprintf(stderr, "%s: failed to open '%s' for writing\n", __func__, fname_out.c_str());
               return false;
           }
           fprintf(stderr, "%s: saving output to '%s'\n", function, fname_out.c_str());
           return true;
       }
   } fout_factory{ (params.fname_out.empty())? "" : params.fname_out[0], fname_inp, params };

   if (!::read_audio_data(fname_inp, pcmf32, pcmf32s, params.diarize)) {
       fprintf(stderr, "error: failed to read audio file\n" );
       hb_retni( 1 );
       return;
   }

   if (!whisper_is_multilingual(ctx)) {
       if (params.language != "en" || params.translate) {
           params.language = "en";
           params.translate = false;
           fprintf(stderr, "%s: WARNING: model is not multilingual, ignoring language and translation options\n", __func__);
       }
   }
   if (params.detect_language) {
       params.language = "auto";
   }

   if (!params.no_prints) {
       // print system information
       fprintf(stderr, "\n");
       fprintf(stderr, "system_info: n_threads = %d / %d | %s\n",
               params.n_threads*params.n_processors, std::thread::hardware_concurrency(), whisper_print_system_info());

       // print some info about the processing
       fprintf(stderr, "\n");
       fprintf(stderr, "processing '%s' (%d samples, %.1f sec), %d threads, %d processors, %d beams + best of %d, lang = %s, task = %s, %stimestamps = %d ...\n",
               __func__, int(pcmf32.size()), float(pcmf32.size())/WHISPER_SAMPLE_RATE,
               params.n_threads, params.n_processors, params.beam_size, params.best_of,
               params.language.c_str(),
               params.translate ? "translate" : "transcribe",
               params.tinydiarize ? "tdrz = 1, " : "",
               params.no_timestamps ? 0 : 1);

       if (params.print_colors) {
           fprintf(stderr, "%s: color scheme: red (low confidence), yellow (medium), green (high confidence)\n", __func__);
       } else if (params.print_confidence) {
           fprintf(stderr, "%s: confidence: highlighted (low confidence), underlined (medium), dim (high confidence)\n", __func__);
       }
       fprintf(stderr, "\n");
   }

   // run the inference
   {
      whisper_full_params wparams = whisper_full_default_params(WHISPER_SAMPLING_GREEDY);

      const bool use_grammar = (!params.grammar_parsed.rules.empty() && !params.grammar_rule.empty());
      wparams.strategy = (params.beam_size > 1 || use_grammar) ? WHISPER_SAMPLING_BEAM_SEARCH : WHISPER_SAMPLING_GREEDY;

      wparams.print_realtime   = false;
      wparams.print_progress   = params.print_progress;
      wparams.print_timestamps = !params.no_timestamps;
      wparams.print_special    = params.print_special;
      wparams.translate        = params.translate;
      wparams.language         = params.language.c_str();
      wparams.detect_language  = params.detect_language;
      wparams.n_threads        = params.n_threads;
      wparams.n_max_text_ctx   = params.max_context >= 0 ? params.max_context : wparams.n_max_text_ctx;
      wparams.offset_ms        = params.offset_t_ms;
      wparams.duration_ms      = params.duration_ms;

      wparams.token_timestamps = params.output_wts || params.output_jsn_full || params.max_len > 0;
      wparams.thold_pt         = params.word_thold;
      wparams.max_len          = params.output_wts && params.max_len == 0 ? 60 : params.max_len;
      wparams.split_on_word    = params.split_on_word;
      wparams.audio_ctx        = params.audio_ctx;

      wparams.debug_mode       = params.debug_mode;

      wparams.tdrz_enable      = params.tinydiarize; // [TDRZ]

      wparams.suppress_regex   = params.suppress_regex.empty() ? nullptr : params.suppress_regex.c_str();

      wparams.initial_prompt   = params.prompt.c_str();

      wparams.greedy.best_of        = params.best_of;
      wparams.beam_search.beam_size = params.beam_size;

      wparams.temperature_inc  = params.no_fallback ? 0.0f : params.temperature_inc;
      wparams.temperature      = params.temperature;

      wparams.entropy_thold    = params.entropy_thold;
      wparams.logprob_thold    = params.logprob_thold;
      wparams.no_speech_thold  = params.no_speech_thold;

      wparams.no_timestamps    = params.no_timestamps;

      wparams.suppress_nst     = params.suppress_nst;

      wparams.vad            = params.vad;
      wparams.vad_model_path = params.vad_model.c_str();

      wparams.vad_params.threshold               = params.vad_threshold;
      wparams.vad_params.min_speech_duration_ms  = params.vad_min_speech_duration_ms;
      wparams.vad_params.min_silence_duration_ms = params.vad_min_silence_duration_ms;
      wparams.vad_params.max_speech_duration_s   = params.vad_max_speech_duration_s;
      wparams.vad_params.speech_pad_ms           = params.vad_speech_pad_ms;
      wparams.vad_params.samples_overlap         = params.vad_samples_overlap;

      whisper_print_user_data user_data = { &params, &pcmf32s, 0 };

      const auto & grammar_parsed = params.grammar_parsed;
      auto grammar_rules = grammar_parsed.c_rules();

      if (use_grammar) {
          if (grammar_parsed.symbol_ids.find(params.grammar_rule) == grammar_parsed.symbol_ids.end()) {
              fprintf(stderr, "%s: warning: grammar rule '%s' not found - skipping grammar sampling\n", __func__, params.grammar_rule.c_str());
          } else {
              wparams.grammar_rules = grammar_rules.data();
              wparams.n_grammar_rules = grammar_rules.size();
              wparams.i_start_rule = grammar_parsed.symbol_ids.at(params.grammar_rule);
              wparams.grammar_penalty = params.grammar_penalty;
          }
      }

      // this callback is called on each new segment
      if (!wparams.print_realtime && !bToString ) {
          wparams.new_segment_callback = fout_factory.print_segment_callback;
          wparams.new_segment_callback_user_data = &user_data;
      }

      /*
      if (wparams.print_progress) {
          wparams.progress_callback = whisper_print_progress_callback;
          wparams.progress_callback_user_data = &user_data;
      }  */

      // in examples below, we do not abort the processing, but we could if the flag is set to true
      // the callback is called before every encoder run - if it returns false, the processing is aborted
      isAborted = false;
      wparams.encoder_begin_callback = [](struct whisper_context *, struct whisper_state *, void * user_data) {
         return !isAborted;
      };

      // the callback is called before every computation - if it returns true, the computation is aborted
      wparams.abort_callback = [](void * user_data) { return isAborted; };

      if( whisper_full_parallel( ctx, wparams, pcmf32.data(), pcmf32.size(),
         params.n_processors ) != 0 ) {
         fprintf( stderr, "Failed to process audio\n" );
         hb_retni( 10 );
         return;
      }
   }

   // output stuff
   {
       // macros to stringify function name
#define output_func(func, ext, param, ...) if (param && fout_factory.open(ext, #func)) {\
func(ctx, fout_factory.fout, params, __VA_ARGS__); \
}
#define output_ext(ext, ...) output_func(output_##ext, "." #ext, params.output_##ext, __VA_ARGS__)

       output_ext(txt, pcmf32s);
       //output_ext(vtt, pcmf32s);
       output_ext(srt, pcmf32s);
       //output_ext(wts, pcmf32s, fname_inp.c_str(), float(pcmf32.size() + 1000)/WHISPER_SAMPLE_RATE, fout_factory.fname_out.c_str());
       //output_ext(csv, pcmf32s);
       output_func(output_json, ".json", params.output_jsn, pcmf32s);
       output_ext(lrc, pcmf32s);
       output_func(output_score, ".score.txt", params.log_score, pcmf32s);

#undef output_ext
#undef output_func

       if (fout_factory.is_stdout && !fout_factory.used_stdout) {
           fprintf(stderr, "warning: '--output-file -' used without any other '--output-*'");
       }
   }

   if( bToString ) {
      std::string s;
      output_string( params, pcmf32s, &s );
      hb_storclen( s.c_str(), s.size(), 3 );
   }

   hb_retni( 0 );
   return;
}

HB_FUNC( LLM_WHISPER_SETCALLBACK )
{
   if( hb_pcount() > 0 && HB_ISCHAR(1) )
      s_pSymTest = hb_dynsymGetCase( hb_parc(1) );
   else
      s_pSymTest = NULL;
}

HB_FUNC( LLM_WHISPER_ABORT )
{
   isAborted = true;
}

HB_FUNC( LLM_WHISPER_PRINT_TIMINGS )
{

   whisper_print_timings(ctx);
}