/*
 * Llama.prg - Harbour wrappers for llama.cpp
 * Test of recognizing audio (wav) files
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 *
 * Usage:
 *   test_wh <path_to_model> <path_to_wav> [cParamsString]
 * You can find a whisper model, appropriate for your needs and resources on
 *    https://huggingface.co/ggerganov/whisper.cpp
 * Parameters must be divided with '~':
 * "otxt~ml=16"
*/

REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866
REQUEST HB_CODEPAGE_UTF8

Function Main( cModel, cWav, cParams )

   LOCAL n1, n2

   IF Empty( cModel ) .OR. !File( cModel ) .OR. Empty( cWav ) .OR. !File( cWav )
      IF Empty( cModel )
         ? "Model name absent."
      ELSEIF Empty( cWav )
         ? "Audio file name absent."
      ELSEIF !File( cModel )
         ? cModel + " not found."
      ELSEIF !File( cWav )
         ? cWav + " not found."
      ENDIF
      ? "Usage:"
      ? "  test_wh <path_to_whisper_model> <path_to_wav> [options]"
      ? "Options:"
      ? llm_whisper_print_usage()
      RETURN Nil
   ENDIF

   IF !Empty( cParams )
      ? "Model parameters accepted"
      llm_Whisper_Set_Params( cParams )
      hb_Memowrit( "test_wh_params.out", llm_whisper_print_usage() )
   ENDIF

   ? Time() + " Loading..."
   n1 := llm_rediron( 1, "stderr.log" )
   n2 := llm_rediron( 2, "stderr.log" )

   IF llm_Whisper_Open_Model( cModel ) != 0
      ? " === Can't init whisper ==="
      RETURN Nil
   ENDIF

   llm_rediroff( 2, n2 )
   llm_rediroff( 1, n1 )

   ? Time() + " " + cWav + " processing..."
   llm_whisper_setcallback( "FCALLBACK" )
   llm_whisper_recognize( cWav )

   llm_whisper_close_model()

   ? Time() + " Done"

   RETURN Nil

FUNCTION FCallBack( s )

   writelog( s )
   writelog( "---" )
   ?? s
   IF Inkey() == 27
      llm_whisper_abort()
   ENDIF

   RETURN Nil

STATIC FUNCTION WriteLog( cText, fname )

   LOCAL nHand

   fname := IIf( fname == Nil, "a.log", fname )
   IF ! File( fname )
      nHand := FCreate( fname )
   ELSE
      nHand := FOpen( fname, 1 )
   ENDIF
   FSeek( nHand, 0, 2 )
   FWrite( nHand, cText + Chr( 10 ) )
   FClose( nHand )

   RETURN nil