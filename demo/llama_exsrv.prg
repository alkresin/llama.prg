/*
 * llama_exsrv.prg
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
*/

REQUEST HB_CODEPAGE_RU1251, HB_CODEPAGE_RU866
REQUEST HB_CODEPAGE_UTF8

STATIC hExt

Memvar lModelOn

FUNCTION Main( ... )

   LOCAL lServerMode := .F., aParams := hb_aParams(), i
   PUBLIC lModelOn := .F.

   FOR i := 1 TO Len( aParams )
      IF Left( aParams[i],4 ) == "dir="
         lServerMode := .T.
         EXIT
      ENDIF
   NEXT

   IF !lServerMode
      IF Empty( aParams )
         ? "Model name absent"
      ELSEIF !File( aParams[1] )
         ? "File " + aParams[1] + " isn't found"
      ELSE
         FTest( aParams[1] )
      ENDIF
      RETURN Nil
   ENDIF

   IF Empty( hExt := esrv_Init( hb_arrayToParams( hb_aParams() ) ) )
      RETURN Nil
   ENDIF

   IF esrv_LogLevel( hExt ) > 0
      i := llm_rediron( 2, "stderr.log" )
   ENDIF

   esrv_Wait( hExt )

   IF lModelOn
      CloseModel()
   ENDIF
   IF esrv_LogLevel( hExt ) > 0
      llm_rediroff( 2, i )
   ENDIF

   RETURN Nil

STATIC FUNCTION FTest( cModelName )

   LOCAL n2, cQue, cAns, cAnswer

   ? hb_fnameName( cModelName ) + "   Loading..."

   n2 := llm_rediron( 2, "stderr.log" )
   IF llm_Open_Model( cModelName ) != 0
      ? " === Can't open " + cModelName + " ==="
      RETURN Nil
   ENDIF
   IF llm_Create_Context() < 0
      ? "Can't create context"
   ELSE
      DO WHILE .T.
         ?
         ACCEPT "> " TO cQue
         IF Empty( cQue )
            EXIT
         ENDIF

         llm_Ask( cQue )
         cAnswer := ""
         DO WHILE ( cAns := llm_GetNextToken() ) != Nil
            cAnswer += cAns
            ?? cAns
            IF Inkey() == 27
               EXIT
            ENDIF
         ENDDO
      ENDDO
      llm_Close_Context()

   ENDIF

   llm_Close_Model()
   llm_rediroff( 2, n2 )

   RETURN Nil

FUNCTION OpenModel( aParams )

   lModelOn := ( llm_Open_Model( aParams[1], Iif( Len(aParams)<1, aParams[2], Nil ) ) == 0 )
   gWritelog( hExt, Time() + " - model " + aParams[1] + " " + Iif( lModelOn, "ok", "no" ) )

   RETURN Iif( lModelOn, "ok", "no" )

FUNCTION CloseModel()

   llm_Close_Model()
   lModelOn := .F.
   RETURN "ok"

FUNCTION CreateContext()

   LOCAL lRes := ( llm_Create_Context() >= 0 )

   gWritelog( hExt, Time() + " - context " + Iif( lRes, "ok", "no" ) )
   RETURN Iif( lRes, "ok", "no" )

FUNCTION CloseContext()

   llm_Close_Context()
   RETURN "ok"

FUNCTION Ask( aParams )

   llm_Ask( aParams[1] )

   RETURN "ok"

FUNCTION GetNextToken( aParams )

   LOCAL xAns, n, s

   IF Len( aParams ) > 0 .AND. Valtype(aParams[1]) == "N" .AND. ( n := aParams[1] ) > 0
      xAns := ""
      DO WHILE --n >= 0
         IF ( s := llm_GetNextToken() ) == Nil
            xAns += "===="
            EXIT
         ELSE
            xAns += s
         ENDIF
      ENDDO
   ELSE
      xAns := llm_GetNextToken()
   ENDIF

   RETURN Iif( xAns == Nil, "====", xAns )
