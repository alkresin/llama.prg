/*
 * Ext
 * A set of routines to launch an external application and keep connection with it
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
 */

STATIC cn := e"\n"
STATIC nInterval := 20
STATIC cLogFile := "extclient.log"
STATIC aExt := {}, nExtId := 0

FUNCTION ecli_Run( cExe, nLog, cDir, cFile )

   LOCAL h := hb_hash(), nSec, cRun
   LOCAL nLogOn, cDirRoot, cFileRoot := "gs"

   nLogOn := Iif( Valtype( nLog ) == "N", nLog, 0 )
   cDirRoot := Iif( Empty( cDir ), hb_DirTemp(), cDir )
   IF !( Right( cDirRoot,1 ) $ "\/" )
      cDirRoot += hb_ps()
   ENDIF

   h["log"] := nLogOn
   h["id"] := ++ nExtId
   h["dir"] := cDirRoot
   h["cb"] := Nil
   h["active"] := .F.
   h["hin"] := -1
   h["hout"] := -1
   h["bufres"] := ""

   IF !Empty( cFile ) .AND. Valtype( cFile ) == "C"
      cFileRoot := cFile
   ENDIF
   IF !conn_Client_Create( h, cDirRoot + cFileRoot, .F. )
      IF h["hin"] >= 0
         FClose( h["hin"] )
      ENDIF
      IF h["hout"]
         FClose( h["hout"] )
      ENDIF
      RETURN .F.
   ENDIF

   cDirRoot = 'dir=' + hb_strShrink( cDirRoot, 1 )
   IF ' ' $ cDirRoot .AND. !( Left(cDirRoot,1) == '"' )
      cDirRoot := '"' + cDirRoot + '"'
   ENDIF

   cRun := cExe + ' ' + cDirRoot + ' type=2 ' + Iif( nLogOn>0, "log="+Str(nLogOn,1), "" ) + ;
      Iif( !Empty(cFile).AND.Valtype(cFile)=="C", " file="+cFile, "" )
   gWritelog( h, cRun )
   ecli_RunApp( cRun )

   nSec := Seconds()
   DO WHILE Seconds() - nSec < 1
      IF !Empty( ecli_CheckAnswer( h ) )
         AAdd( aExt, h )
         RETURN h
      ENDIF
      gs_Sleep( nInterval*2 )
   ENDDO

   FClose( h["hin"] )
   FClose( h["hout"] )
   h["active"] := .F.

   RETURN Nil

FUNCTION ecli_Close( h )

   LOCAL i, id := h["id"]

   SendOut( h, '["endapp"]', .T. )
   gs_Sleep( nInterval*2 )

   conn_Exit( h )
   gs_Sleep( nInterval*2 )

   FOR i := 1 TO Len( aExt )
      IF aExt[i]["id"] == id
         hb_ADel( aExt, i, .T. )
         EXIT
      ENDIF
   NEXT

   RETURN Nil

FUNCTION ecli_RunProc( h, cFunc, aParams )

   SendOut( h, hb_jsonEncode( { "runproc", cFunc, hb_jsonEncode( aParams ) } ) )

   RETURN Nil

FUNCTION ecli_RunFunc( h, cFunc, aParams, lNoWait )

   LOCAL cRes := SendOut( h, hb_jsonEncode( { "runfunc", cFunc, hb_jsonEncode( aParams ) } ), lNoWait ), xRes

   IF !Empty( cRes )
      hb_jsonDecode( cRes, @xRes )
   ENDIF

   RETURN xRes

FUNCTION ecli_CheckAnswer( h )

   LOCAL cRes := conn_CheckOut( h ), xRes

   IF !Empty( cRes )
      hb_jsonDecode( cRes, @xRes )
   ENDIF

   RETURN xRes

FUNCTION ecli_SetVar( h, cVarName, cValue )

   SendOut( h, hb_jsonEncode( { "setvar", cVarName, cValue } ) )

   RETURN Nil

FUNCTION ecli_GetVar( h, cVarName )

   LOCAL cRes := SendOut( h, hb_jsonEncode( { "getvar", cVarName } ) )

   RETURN Substr( cRes,2,Len(cRes)-2 )

STATIC FUNCTION SendOut( h, s, lNoWait )

   LOCAL cRes

   gWritelog( h, " " + s )
   cRes := conn_Send2SocketOut( h, "+" + s + cn, lNoWait )

   RETURN Iif( Empty(cRes), "", cRes )

STATIC FUNCTION SendIn( h, s )

   conn_Send2SocketIn( h, "+" + s + cn )

   RETURN Nil

FUNCTION MainHandler( h )

   LOCAL arr, cBuffer

   cBuffer := conn_GetRecvBuffer( h )

   gWritelog( h, cBuffer )

   hb_jsonDecode( cBuffer, @arr )
   IF Valtype(arr) != "A" .OR. Empty(arr)
      SendIn( h, '"Wrong"' )
      RETURN Nil
   ENDIF
   SendIn( h, '"Ok"' )

   RETURN Nil

FUNCTION gWritelog( h, s )

   LOCAL nHand, cFile

   IF h["log"] > 0
      cFile := h["dir"] + cLogFile
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

EXIT PROCEDURE ECLI_EXIT

   LOCAL i

   FOR i := Len( aExt ) TO 1 STEP -1
      ecli_Close( aExt[i] )
   NEXT

   RETURN

#pragma BEGINDUMP

#if defined(HB_OS_UNIX) || defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
  #include <unistd.h>
  #include <sys/time.h>
  #include <sys/timeb.h>
#else
  #include <windows.h>
#endif

#include "hbapi.h"

#if defined(HB_OS_UNIX) || defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
HB_FUNC( ECLI_RUNAPP )
{
   //hb_retl( g_spawn_command_line_async( hb_parc(1), NULL ) );
   char scmd[2048];
   int nLen = hb_parclen( 1 );

   memcpy( scmd, hb_parc(1), nLen );
   scmd[nLen] = ' ';
   scmd[nLen+1] = '&';
   scmd[nLen+2] = '\0';
   hb_retl( system( scmd ) > 0 );
}
#else
HB_FUNC( ECLI_RUNAPP )
{
   STARTUPINFO si;
   PROCESS_INFORMATION pi;
#ifdef UNICODE
   TCHAR wc1[CMDLENGTH];
#endif
   BOOL bRes;

   ZeroMemory( &si, sizeof(si) );
   si.cb = sizeof(si);
   si.wShowWindow = SW_SHOW; // SW_HIDE;
   si.dwFlags = STARTF_USESHOWWINDOW;
   ZeroMemory( &pi, sizeof(pi) );

#ifdef UNICODE
   MultiByteToWideChar( GetACP(), 0, hb_parc(1), -1, wc1, CMDLENGTH );
   bRes = CreateProcess( NULL,   // No module name (use command line)
       wc1,            // Command line
       NULL,           // Process handle not inheritable
       NULL,           // Thread handle not inheritable
       FALSE,          // Set handle inheritance to FALSE
       CREATE_NO_WINDOW,  // No creation flags
       NULL,           // Use parent's environment block
       NULL,           // Use parent's starting directory
       &si,            // Pointer to STARTUPINFO structure
       &pi );          // Pointer to PROCESS_INFORMATION structure
#else
   bRes = CreateProcess( NULL,   // No module name (use command line)
       (LPTSTR)hb_parc(1),  // Command line
       NULL,           // Process handle not inheritable
       NULL,           // Thread handle not inheritable
       FALSE,          // Set handle inheritance to FALSE
       CREATE_NO_WINDOW,  // No creation flags
       NULL,           // Use parent's environment block
       NULL,           // Use parent's starting directory
       &si,            // Pointer to STARTUPINFO structure
       &pi );          // Pointer to PROCESS_INFORMATION structure
#endif
   hb_retl( bRes != 0 );
}

#endif

static void sleep_ns( long int milliseconds )
{
#if defined(HB_OS_UNIX) || defined( HB_OS_UNIX ) || defined( HB_OS_BSD )
   struct timeval tv;
   tv.tv_sec = milliseconds / 1000;
   tv.tv_usec = milliseconds % 1000 * 1000;
   select(0, NULL, NULL, NULL, &tv);
#else
   Sleep( milliseconds );
#endif
}

HB_FUNC( GS_SLEEP )
{
   sleep_ns( hb_parni(1) );
}

#pragma ENDDUMP