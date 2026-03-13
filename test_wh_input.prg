/*
 * Llama.prg - Harbour wrappers for llama.cpp,
 * Test of recognizing input audio stream
 *
 * Copyright 2026 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 *
 * Usage:
 *   test5 <path_to_model>
 * You can find a whisper model, appropriate for your needs and resources on
 *    https://huggingface.co/ggerganov/whisper.cpp
*/

FUNCTION Main( cModel )

   LOCAL nRes, nKey, n1, n2, lFirst := .T.
   LOCAL cWhisperParams := "nt~np~l=auto"

   IF Empty( cModel ) .OR. !File( cModel )
      ? "Wrong or absent parameters"
      ? "Usage:"
      ? "  test5 <path_to_whisper_model"
      RETURN Nil
   ENDIF

   ? Time() + " Loading..."
#ifdef __PLATFORM__UNIX
   n1 := llm_rediron( 1, "stderr.log" )
   n2 := llm_rediron( 2, "stderr.log" )
#endif
   IF llm_Whisper_Open_Model( cModel ) != 0
      ? " === Can't init whisper ==="
      RETURN Nil
   ENDIF

#ifdef __PLATFORM__UNIX
   llm_rediroff( 2, n2 )
   llm_rediroff( 1, n1 )
#endif

   llm_Whisper_Set_Params( cWhisperParams )
   llm_whisper_setcallback( "FCALLB1" )

   DO WHILE .T.
      IF lFirst
         ? Time() + " Press Space to start recording, ESC - to Quit"
      ELSE
         lFirst := .F.
         ? "Press Space to continue, ESC - to Quit"
      ENDIF
      DO WHILE ( nKey := Inkey(0) ) != 32 .AND. nKey != 27 .AND. nKey != 13; ENDDO
      IF nKey == 27
         EXIT
      ENDIF

      ? Time() + " Recording... Press Space to pause, ESC - to Quit"
      CLEAR TYPEAHEAD
      IF !RecordAndRecognize( "test_wh_input.wav", 30 )
         EXIT
      ENDIF
   ENDDO
   ? "Bye"

#ifdef __PLATFORM__UNIX
   n1 := llm_rediron( 1, "stderr.log" )
   n2 := llm_rediron( 2, "stderr.log" )
#endif

   llm_whisper_close_model()

#ifdef __PLATFORM__UNIX
   llm_rediroff( 2, n2 )
   llm_rediroff( 1, n1 )
#endif

   RETURN Nil

STATIC FUNCTION RecordAndRecognize( cFile, nSeconds )

   LOCAL pDev, nKey, nSec, cText

   IF Empty( pDev := ma_device_capture_init( cFile, 16000, 1 ) )
      ? "Capture init failed"
      RETURN .F.
   ENDIF

   ? "Capture init Ok"
   ma_device_start( pDev )
   ? "Capture started"
   nSec := Seconds()

   DO WHILE Seconds() - nSec < 30
      nKey := Inkey( 0.1 )
      IF nKey == 27 .OR. nKey == 32
         EXIT
      ENDIF
   ENDDO

   ma_device_stop( pDev )
   ma_device_capture_uninit( pDev )

   ? Time() + " Recognizing..."
   ?
   llm_whisper_recognize( cFile, @cText )
   IF Empty( cText )
      ? "No result"
   ELSE
      ? cText
      WriteLog( cText )
   ENDIF

   RETURN nKey != 27

FUNCTION FCallB1( s )

   ?? s
   IF Inkey() == 27
      llm_whisper_abort()
   ENDIF

   RETURN Nil

STATIC FUNCTION WriteLog( cText )

   LOCAL nHand, fname := "test_wh_input.out"

   IF ! File( fname )
      nHand := FCreate( fname )
   ELSE
      nHand := FOpen( fname, 1 )
   ENDIF
   FSeek( nHand, 0, 2 )
   FWrite( nHand, cText + Chr( 10 ) )
   FClose( nHand )

   RETURN nil