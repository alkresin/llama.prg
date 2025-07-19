/*
 *
 * Llama.prg - Harbour wrappers for llama.cpp,
 * which allows chatting with local AI models
 *
 * This work is based on llama.cpp project
 * https://github.com/ggerganov/llama.cpp
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
*/

#if defined(__linux__) || defined(__unix__)
   #include <unistd.h>
#else
   #include <io.h>
#endif

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "item.api"

HB_FUNC( LLM_REDIRON )
{
   int istd = ( HB_ISNIL( 1 ) ) ? 1 : hb_parni( 1 );
   int fd;

   fflush( ( istd == 1 ) ? stdout : stderr );
   fd = dup( fileno( ( istd == 1 ) ? stdout : stderr ) );
   freopen( hb_parc( 2 ), "w", ( istd == 1 ) ? stdout : stderr );
   hb_retni( fd );
}

HB_FUNC( LLM_REDIROFF )
{
#if !defined(_MSC_VER)
   int istd = ( HB_ISNIL( 1 ) ) ? 1 : hb_parni( 1 );
   int fd;

   fflush( ( istd == 1 ) ? stdout : stderr );

   if( HB_ISNIL( 2 ) )
   {
      fclose( ( istd == 1 ) ? stdout : stderr );
   }
   else
   {
      fd = hb_parni( 2 );
      dup2( fd, fileno( ( istd == 1 ) ? stdout : stderr ) );
      close( fd );
      clearerr( ( istd == 1 ) ? stdout : stderr );
   }
#endif
}
