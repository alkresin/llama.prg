/*
 *
 * Llama.prg - Harbour wrappers for llama.cpp,
 * which allows chatting with local AI models
 *
 * This work is based on llama.cpp project
 * https://github.com/ggerganov/llama.cpp
 *
 * Copyright 2024-2025 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
*/

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "item.api"

extern void llm_writelog( const char * sFile, const char * sTraceMsg, ... );
extern int llm_open_model( int argc, char **argv );
extern int llm_create_context( void );
extern int llm_ask_0( const char * szPrompt );
extern const char * llm_getnexttoken_0( void );
extern int llm_ask( const char * szPrompt );
extern const char * llm_getnexttoken( void );
extern void llm_close_model( void );
extern void llm_close_context( void );

HB_FUNC( LLM_OPEN_MODEL )
{

   char *p1 = "", *p2 = "-m", *p3 = (char*) hb_parc(1);
   char ** argv = (char **) malloc( sizeof(char*) * 3 );
   argv[0] = p1; argv[1] = p2; argv[2] = p3;

   if( llm_open_model( 3, argv ) ) {
      hb_retni( 1 );
      return;
   }

   free( argv );
   hb_retni( 0 );
}

HB_FUNC( LLM_CREATE_CONTEXT )
{

   if( llm_create_context() )
      hb_retni( -1 );
   else
      hb_retni( 0 );

}

HB_FUNC( LLM_ASK_0 )
{

   llm_ask_0( hb_parc( 1 ) );
}

HB_FUNC( LLM_GETNEXTTOKEN_0 )
{

   const char * szToken = llm_getnexttoken_0();
   if( szToken )
      hb_retc( szToken );
   else
      hb_ret();
}

HB_FUNC( LLM_ASK )
{

   llm_ask( hb_parc( 1 ) );
}

HB_FUNC( LLM_GETNEXTTOKEN )
{

   const char * szToken = llm_getnexttoken();
   if( szToken )
      hb_retc( szToken );
   else
      hb_ret();
}

HB_FUNC( LLM_CLOSE_CONTEXT )
{
   llm_close_context();
}

HB_FUNC( LLM_CLOSE_MODEL )
{
   llm_close_model();
}