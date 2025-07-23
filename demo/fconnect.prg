/*
 * Ext
 * A set of routines to launch an external application and keep connection with it
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
*/

#include "fileio.ch"
#define PROTOCOL_VER "1.1"
#define BUFFLEN   2048
#define SERVER_ID 1
#define CLIENT_ID 2

STATIC cVersion := "1.0"
STATIC nMyId, nHisId

FUNCTION conn_SetCallBack( h, b )

   h["cb"] := b
   RETURN Nil

FUNCTION conn_SetVersion( s )

   cVersion := s
   RETURN Nil

FUNCTION conn_Read( h, lOut )

   LOCAL n, nPos, s := ""
   LOCAL han := Iif( lOut, h["hout"], h["hin"] )
   LOCAL cBuffer := Space( BUFFLEN )

   FSeek( han, 1, 0 )
   DO WHILE ( n := FRead( han, @cBuffer, Len(cBuffer ) ) ) > 0
      IF ( nPos := At( Chr(10), cBuffer ) ) > 0
         s += Left( cBuffer, nPos-1 )
         EXIT
      ELSEIF n < Len(cBuffer )
         s += Left( cBuffer, n )
         EXIT
      ELSE
         s += cBuffer
      ENDIF
   ENDDO

   h["bufres"] := s

   RETURN Len( s )

FUNCTION conn_GetRecvBuffer( h )

   RETURN Substr( h["bufres"], 2 )

FUNCTION conn_Send( h, lOut, cLine )

   LOCAL han := Iif( lOut, h["hout"], h["hin"] )

   IF h["active"]
      FSeek( han, 1, 0 )
      FWrite( han, cLine )
      FSeek( han, 0, 0 )
      FWrite( han, Chr(nMyId) )
   ENDIF

   RETURN Nil

FUNCTION conn_Send2SocketIn( h, s )

   IF h["active"]
      conn_Send( h, .F., s )
   ENDIF

   RETURN Nil

FUNCTION conn_Send2SocketOut( h, s, lNoWait )

   LOCAL cAns

   IF h["active"]
      conn_Send( h, .T., s )
      IF Empty( lNoWait )
         DO WHILE h["active"]
            conn_CheckIn( h )
            IF !Empty( cAns := conn_CheckOut( h ) )
               RETURN cAns
            ENDIF
            gs_Sleep( 2 )
            IF !Empty( h["cb"] )
               Eval( h["cb"] )
            ENDIF
         ENDDO
      ENDIF
   ENDIF

   RETURN Nil

FUNCTION conn_CheckIn( h )

   LOCAL hIn := h["hin"], bufIn := Space( 10 )

   IF h["active"]
      FSeek( hIn, 0, 0 )
      IF FRead( hIn, @bufIn, 1 ) > 0 .AND. Asc( bufIn ) == nHisId
         gWritelog( h, "Checkin" )
         IF conn_Read( h, .F. ) > 0
            MainHandler( h )
         ENDIF
         RETURN .T.
      ENDIF
   ENDIF
   RETURN .F.

FUNCTION conn_CheckOut( h )

   LOCAL hOut := h["hout"], bufOut := Space( 10 )

   IF h["active"]
      FSeek( hOut, 0, 0 )
      IF FRead( hOut, @bufOut, 1 ) > 0 .AND. Asc( bufOut ) == nHisId
         conn_Read( h, .T. )
         gWritelog( h, "Checkout: " + conn_GetRecvBuffer( h ) )
         RETURN conn_GetRecvBuffer( h )
      ENDIF
   ENDIF
   RETURN Nil

FUNCTION conn_Client_Create( h, cFile )

   LOCAL handlIn, handlOut, cFile1, cFile2

   nMyId := CLIENT_ID
   nHisId := SERVER_ID

   cFile1 := cFile + ".gs1"
   cFile2 := cFile + ".gs2"

   handlIn := FCreate( cFile1 )
   FClose( handlIn )

   handlOut := FCreate( cFile2 )
   FClose( handlOut )

   handlIn := FOpen( cFile1, FO_READWRITE + FO_SHARED )
   gwritelog( h, "Open in " + cFile1 + " " + str(handlIn) )
   handlOut := FOpen( cFile2, FO_READWRITE + FO_SHARED )
   gwritelog( h, "Open out " + cFile2 + " " + str(handlOut) )

   h["active"] := ( handlIn >= 0 .AND. handlOut >= 0 )
   h["hin"] := handlIn
   h["hout"] := handlOut

   conn_Send( h, .F., "+v" + cVersion + "/" + PROTOCOL_VER + Chr(10) )
   conn_Send( h, .T., "+Ok" + Chr(10) )

   RETURN h["active"]

FUNCTION conn_Server_Connect( h, cFile )

   LOCAL sRes, handlIn, handlOut, cFile1, cFile2

   nMyId := SERVER_ID
   nHisId := CLIENT_ID

   cFile1 := cFile + ".gs1"
   cFile2 := cFile + ".gs2"

   handlOut := FOpen( cFile1, FO_READWRITE + FO_SHARED )
   gwritelog( h, "Open out " + cFile1 + " " + str(handlOut) )
   handlIn := FOpen( cFile2, FO_READWRITE + FO_SHARED )
   gwritelog( h, "Open in " + cFile2 + " " + str(handlIn) )

   h["active"] := ( handlIn >= 0 .AND. handlOut >= 0 )
   h["hin"] := handlIn
   h["hout"] := handlOut

   IF h["active"] .AND. conn_Read( h, .T. ) > 0
      sRes := conn_GetRecvBuffer( h )
   ENDIF

   RETURN sRes

PROCEDURE conn_Exit( h )

   h["active"] := .F.
   FClose( h["hin"] )
   FClose( h["hout"] )

   RETURN