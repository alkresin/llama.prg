/*
 * Ext
 * A set of routines to launch an external application and keep connection with it
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
*/

#define GUIS_VERSION   "1.4"

STATIC nInterval := 20
STATIC cn := e"\n"
STATIC cLogFile := "extserver.log"

FUNCTION esrv_Init( ... )

   LOCAL i, aParams := hb_aParams(), x
   LOCAL nLogOn := 0, cFileRoot := "gs", cDir
   LOCAL h := hb_hash()

   FOR i := 1 TO Len( aParams )
      IF ( x := Left( aParams[i],4 ) ) == "dir="
         cDir := Substr( aParams[i], 5 )
         IF Left( cDir,1 ) == '"'
            cDir := Substr( cDir, 2, Len(cDir)-2 )
         ENDIF
      ELSEIF x == "log="
         nLogOn := Val( Substr( aParams[i], 5 ) )
      ELSEIF Left( aParams[i],5 ) == "file="
         cFileRoot := Substr( aParams[i], 6 )
      ENDIF
   NEXT

   IF Empty( cDir )
      cDir := hb_DirTemp()
   ENDIF
   IF !( Right( cDir,1 ) $ "\/" )
      cDir += hb_ps()
   ENDIF

   h["log"] := nLogOn
   h["dir"] := cDir
   h["end"] := .F.
   h["cb"] := Nil

   conn_SetVersion( GUIS_VERSION )
   gWritelog( h, "Connect via files "+ cDir + cFileRoot + ".*" )
   IF Empty( conn_Server_Connect( h, cDir + cFileRoot ) )
      RETURN Nil
   ENDIF
   SendIn( h, '"ok"' )

   RETURN h

FUNCTION esrv_LogLevel( h, nLogLevel )

   LOCAL n := h["log"]

   IF Valtype( nLogLevel ) == "N"
      h["log"] := nLogLevel
   ENDIF

   RETURN n

FUNCTION esrv_Wait( h )

   DO WHILE !h["end"]
      gs_Sleep( nInterval )
      conn_CheckIn( h )
   ENDDO
   gWritelog( h, "esrv_Wait: exit" )

   RETURN Nil

FUNCTION esrv_RunProc( h, cFunc, aParams )

   SendOut( h, hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( aParams ) } ) )

   RETURN Nil

STATIC FUNCTION Parse( h, arr )

   LOCAL cCommand := Lower( arr[1] ), c := Left( cCommand, 1 ), arrp
   LOCAL o, lErr := .F., cRes := "", cFunc, xRes

   gwritelog( h, "Command: " + cCommand )
   SWITCH c
   CASE 's'
      IF cCommand == "setvar"
         lErr := ( Len(arr)<3 )
         IF !lErr
            SendIn( h, '"Ok"' )
            IF !__mvExist( cRes := Upper(arr[2]) )
               __mvPublic( cRes )
            ENDIF
            __mvPut( cRes, arr[3] )
         ENDIF
      ENDIF
      EXIT

   CASE 'g'
      IF cCommand == "getver"
         lErr := ( Len(arr)<2 )
         IF !lErr
            SendIn( h, hb_jsonEncode(gVersion(arr[2])) )
         ENDIF
      ELSEIF cCommand == "getvar"
         lErr := ( Len(arr)<2 )
         IF !lErr
            IF __mvExist( cRes := Upper(arr[2]) )
               cRes := __mvGet( cRes )
            ELSE
               cRes := Nil
            ENDIF
            SendIn( h, hb_jsonEncode(cRes) )
         ENDIF
      ENDIF
      EXIT

   CASE 'e'
      IF cCommand == "exit" .OR. cCommand == "endapp"
         h["end"] := .T.
         SendIn( h, '"Ok"' )
      ENDIF
      EXIT

   CASE 'r'
      IF cCommand == "runproc"

         cFunc := Lower( arr[2] )
         IF hb_isFunction( cFunc )
            SendIn( h, '"Ok"' )
            xRes := &( "{|a|"+cFunc+"(a)}" )
            IF Len( arr ) > 2
               hb_jsonDecode( arr[3], @arrp )
            ENDIF
            Eval( xRes, arrp )
         ELSE
            lErr := .T.
         ENDIF

      ELSEIF cCommand == "runfunc"

         cFunc := Lower( arr[2] )
         IF hb_isFunction( cFunc )
            xRes := &( "{|a|"+cFunc+"(a)}" )
            IF Len( arr ) > 2
               hb_jsonDecode( arr[3], @arrp )
            ENDIF
            xRes := Eval( xRes, arrp )
            SendIn( h, hb_jsonEncode(xRes) )
         ELSE
            lErr := .T.
         ENDIF
      ENDIF
      EXIT

   OTHERWISE
      lErr := .T.

   END

   IF lErr
      SendIn( h, '"Err"' )
   ENDIF

   RETURN !lErr

FUNCTION MainHandler( h )

   LOCAL arr, cBuffer

   cBuffer := conn_GetRecvBuffer( h )

   gWritelog( h, cBuffer )

   hb_jsonDecode( cBuffer, @arr )
   IF Valtype(arr) != "A" .OR. Empty(arr)
      SendIn( h, '"Wrong"' )
      RETURN Nil
   ENDIF

   IF !Parse( h, arr )
      gWritelog( h, "Parsing error" )
   ENDIF

   RETURN Nil

STATIC FUNCTION SendOut( h, s )

   LOCAL cRes
   gWritelog( h, " " + s )

   cRes := conn_Send2SocketOut( h, "+" + s + cn )

   RETURN Iif( Empty(cRes), "", cRes )

STATIC FUNCTION SendIn( h, s )

   conn_Send2SocketIn( h, "+" + s + cn )

   RETURN Nil

STATIC FUNCTION gVersion( n )
   RETURN Iif( n==0, GUIS_VERSION, "hbExtServer " + GUIS_VERSION )

FUNCTION gWritelog( h, s )

   LOCAL nHand, cFile

   IF h["log"] > 0
      cFile := h["dir"] + cLogFile
      //IF ' ' $ cFile .AND. !( Left(cFile,1) == '"' )
      //   cFile := '"' + cFile + '"'
      //ENDIF
      IF ! File( cFile )
         nHand := FCreate( cFile )
      ELSE
         nHand := FOpen( cFile, 1 )
      ENDIF
      FSeek( nHand, 0, 2 )
      FWrite( nHand, s + cn )
      FClose( nHand )
   ENDIF
   RETURN Nil

#pragma BEGINDUMP

#if defined(HB_OS_UNIX) || defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
  #include <unistd.h>
  #include <sys/time.h>
  #include <sys/timeb.h>
#else
  #include <windows.h>
#endif

#include "hbapi.h"

static void sleep_ns( long int milliseconds )
{
#if defined(HB_OS_WIN_32) || defined( HB_OS_WIN )
   Sleep( milliseconds );
#else
   struct timeval tv;
   tv.tv_sec = milliseconds / 1000;
   tv.tv_usec = milliseconds % 1000 * 1000;
   select(0, NULL, NULL, NULL, &tv);
#endif
}

#include "hbapi.h"

HB_FUNC( GS_SLEEP )
{
   sleep_ns( hb_parni(1) );
}

#pragma ENDDUMP