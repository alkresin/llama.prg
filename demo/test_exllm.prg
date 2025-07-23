
#include "hwgui.ch"

#define S_INIT            0
#define S_MODULE_STARTED  1
#define S_MODEL_PARAMS    2
#define S_MODEL_LOADING   3
#define S_MODEL_LOADED    4
#define S_CNT_CREATING    5
#define S_CNT_CREATED     6
#define S_ASKING          7
#define S_GETTOKEN        8

#define HPANE_HEIGHT     52

#define CLR_WHITE    0xffffff
#define CLR_BLACK    0
#define CLR_DBLUE    0x614834
#define CLR_DGRAY1   0x222222
#define CLR_DGRAYA   0x404040
#define CLR_DGRAY2   0x555555
#define CLR_DGRAY3   0x888888
#define CLR_LGRAY1   0xdddddd

REQUEST HB_CODEPAGE_UTF8

STATIC nStatus := S_INIT
STATIC hExt
STATIC oTimer, nInterval := 100
STATIC aModels := {}, cCurrModel
STATIC cImgPath, cImgPrefix
STATIC nLogLevel := 0
STATIC n_ctx := 512, n_predict := -1, temp := 0.8, penalty_r := 1.1, top_k := 40, top_p := 0.95, penalize_nl := 0, min_p := 0.05
STATIC oFontMain

FUNCTION Main
   LOCAL oMain, oBoa, oBtnMod, oBtnAsk, oBtnReset
   LOCAL aCorners := { 4,4,4,4 }
   LOCAL aStylesBtn := { HStyle():New( { CLR_DGRAY2 }, 1, aCorners ), ;
      HStyle():New( { CLR_WHITE }, 2, aCorners ), ;
      HStyle():New( { CLR_DGRAY3 }, 1, aCorners ) }

   IF hwg__isUnicode()
      hb_cdpSelect( "UTF8" )
   ENDIF

   IniRead( "models.ini" )

   INIT WINDOW oMain MAIN TITLE "Llama.prg" AT 200,0 SIZE 600,350 FONT oFontMain

   @ 4, 0 BOARD oBoa OF oMain SIZE oMain:nWidth-12, HPANE_HEIGHT BACKCOLOR CLR_DGRAYA ;
      FONT oFontMain ON SIZE {|o,x,y|o:Move( ,, x-12 ) }
      // ANCHOR_LEFTABS+ANCHOR_RIGHTABS

   @ 8, 6 DRAWN oBtnMod OF oBoa SIZE 140, 40 COLOR CLR_WHITE ;
      HSTYLES aStylesBtn TEXT 'Load model' ON CLICK {||LoadModel()}

   @ 160, 6 DRAWN oBtnAsk OF oBoa SIZE 140, 40 COLOR CLR_WHITE ;
      HSTYLES aStylesBtn TEXT '' ON CLICK {||Ask()}

   @ 310, 6 DRAWN oBtnReset OF oBoa SIZE 140, 40 COLOR CLR_WHITE ;
      HSTYLES aStylesBtn TEXT '' ON CLICK {||Reset()}

   @ oBoa:nWidth-60, 6 DRAWN OF oBoa SIZE 48, 40 COLOR CLR_WHITE ;
      HSTYLES aStylesBtn TEXT 'Exit' ON CLICK {||oMain:Close()}
      ATail(oBoa:aDrawn):Anchor := ANCHOR_RIGHTABS

   StatusChanging( S_INIT )

   ACTIVATE WINDOW oMain

   RETURN Nil

STATIC FUNCTION AddEdit()

   LOCAL oMain := HWindow():Getmain(), oBoa := oMain:oBoa, oEdit1, oEdit2
   LOCAL nWinHeight := hwg_GetClientRect( oMain:handle )[4]

   @ 4, HPANE_HEIGHT HCEDIT oEdit1 OF oMain SIZE oBoa:nWidth, 80 NO VSCROLL ;
      ON SIZE ANCHOR_LEFTABS+ANCHOR_RIGHTABS
   oEdit1:bColorCur := oEdit1:bColor

   @ 4, HPANE_HEIGHT + oEdit1:nHeight + 3 HCEDIT oEdit2 OF oMain ;
      SIZE oBoa:nWidth, nWinHeight - HPANE_HEIGHT - oEdit1:nHeight - 3 - 4 BACKCOLOR CLR_LGRAY1 ;
      ON SIZE ANCHOR_TOPABS+ANCHOR_BOTTOMABS+ANCHOR_LEFTABS+ANCHOR_RIGHTABS
   oEdit2:bColorCur := oEdit2:bColor
   oEdit2:SetWrap( .T. )
   oEdit2:lReadOnly := .T.
   IF hwg__isUnicode()
      oEdit1:lUtf8 := .T.
      oEdit2:lUtf8 := .T.
   ENDIF

   @ 4,HPANE_HEIGHT + oEdit1:nHeight SPLITTER OF oMain SIZE oBoa:nWidth,3 ;
      DIVIDE {oEdit1} FROM {oEdit2} ON SIZE ANCHOR_LEFTABS+ANCHOR_RIGHTABS

   hwg_SetFocus( oEdit1:handle )

   RETURN Nil

STATIC FUNCTION RunModule()

#ifdef __PLATFORM__UNIX
   LOCAL cExe := "./llama_exsrv"
#else
   LOCAL cExe := "llama_exsrv.exe"
#endif

   IF !Empty( hExt := ecli_run( cExe, nLogLevel,, "ggg" ) )
      StatusChanging( S_MODULE_STARTED )
      SET TIMER oTimer OF HWindow():Getmain() VALUE 100 ACTION {||TimerFunc()}
   ELSE
      hwg_MsgStop( "Failed to start module" )
   ENDIF

   RETURN Nil

STATIC FUNCTION LoadModel()

   LOCAL oDlg, oBoa, lRes := .T., oCombo1, am, i, sParams := ""
   LOCAL oEdit1, oEdit2, oEdit5, oEdit6, oEdit7, oEdit8, oEdit9, oEdit10

   LOCAL aCorners := { 4,4,4,4 }
   LOCAL aStyles := { HStyle():New( { CLR_DGRAY2 }, 1, aCorners ), ;
      HStyle():New( { CLR_LGRAY1 }, 2, aCorners ), ;
      HStyle():New( { CLR_DGRAY3 }, 1, aCorners ) }
   LOCAL bOk := {||
      cCurrModel := aModels[oCombo1:Value(),1]
      IF oEdit1:Value != n_ctx
         n_ctx := oEdit1:Value
         sParams += 'c=' + Ltrim(Str(n_ctx)) + "~"
      ENDIF
      IF oEdit2:Value != n_predict
         n_predict := oEdit2:Value
         sParams += 'n=' + Ltrim(Str(n_predict)) + "~"
      ENDIF
      IF oEdit5:Value != temp
         temp := oEdit5:Value
         sParams += 'temp=' + Ltrim(Str(temp)) + "~"
      ENDIF
      IF oEdit6:Value != penalty_r
         penalty_r := oEdit6:Value
         sParams += 'repeat-penalty=' + Ltrim(Str(penalty_r)) + "~"
      ENDIF
      IF oEdit7:Value != top_k
         top_k := oEdit7:Value
         sParams += 'top-k=' + Ltrim(Str(top_k)) + "~"
      ENDIF
      IF oEdit8:Value != top_p
         top_p := oEdit8:Value
         sParams += 'top-p=' + Ltrim(Str(top_p)) + "~"
      ENDIF
      IF oEdit9:Value != min_p
         min_p := oEdit9:Value
         sParams += 'min-p=' + Ltrim(Str(min_p)) + "~"
      ENDIF
      IF oEdit10:Value != penalize_nl
         penalize_nl := oEdit10:Value
         sParams += 'penalize-nl=' + Iif(penalize_nl==0,'0','1') + "~"
      ENDIF
      lRes := .T.
      oDlg:Close()
      RETURN Nil
   }

   IF nStatus > S_MODULE_STARTED .AND. nStatus <= S_MODEL_LOADING
      RETURN Nil
   ELSEIF nStatus >= S_MODEL_LOADED
      CloseModel()
      RETURN Nil
   ENDIF

   IF Empty( aModels )
      hwg_MsgStop( "Check your models.ini" )
      RETURN Nil
   ENDIF

   INIT DIALOG oDlg TITLE "Load model" AT 0,0 SIZE 400, 440

   @ 0, 0 BOARD oBoa SIZE oDlg:nWidth, oDlg:nHeight FONT HWindow():Getmain():oFont ;
      BACKCOLOR CLR_DGRAYA ON SIZE {|o,x,y|o:Move( ,, x, y )}

   am := Array( Len( aModels ) )
   FOR i := 1 TO Len( am )
      am[i] := hb_fnameName( aModels[i,1] )
   NEXT
   @ 20, 16 DRAWN COMBO oCombo1 ITEMS am SIZE 360, 28 BACKCOLOR CLR_WHITE INIT 1

   @ 10, 100 DRAWN SIZE 100, 28 COLOR CLR_WHITE BACKCOLOR CLR_DGRAYA TEXT 'n_ctx'
   @ 110,100 DRAWN EDIT oEdit1 CAPTION n_ctx PICTURE "9999" SIZE 80, 28

   @ 200,100 DRAWN SIZE 100, 28 COLOR CLR_WHITE BACKCOLOR CLR_DGRAYA TEXT 'n_predict'
   @ 300,100 DRAWN EDIT oEdit2 CAPTION n_predict PICTURE "9999" SIZE 80, 28

   @ 10, 180 DRAWN SIZE 100, 28 COLOR CLR_WHITE BACKCOLOR CLR_DGRAYA TEXT 'temp'
   @ 110,180 DRAWN EDIT oEdit5 CAPTION temp PICTURE "999.9" SIZE 80, 28

   @ 200, 180 DRAWN SIZE 100, 28 COLOR CLR_WHITE BACKCOLOR CLR_DGRAYA TEXT 'penalty_r'
   @ 300,180 DRAWN EDIT oEdit6 CAPTION penalty_r PICTURE "99.9" SIZE 80, 28

   @ 10, 220 DRAWN SIZE 100, 28 COLOR CLR_WHITE BACKCOLOR CLR_DGRAYA TEXT 'top_k'
   @ 110,220 DRAWN EDIT oEdit7 CAPTION top_k PICTURE "999" SIZE 80, 28

   @ 200, 220 DRAWN SIZE 100, 28 COLOR CLR_WHITE BACKCOLOR CLR_DGRAYA TEXT 'top_p'
   @ 300,220 DRAWN EDIT oEdit8 CAPTION top_p PICTURE "99.99" SIZE 80, 28

   @ 10, 260 DRAWN SIZE 100, 28 COLOR CLR_WHITE BACKCOLOR CLR_DGRAYA TEXT 'min_p'
   @ 110,260 DRAWN EDIT oEdit9 CAPTION min_p PICTURE "9.99" SIZE 80, 28

   @ 200, 260 DRAWN SIZE 100, 28 COLOR CLR_WHITE BACKCOLOR CLR_DGRAYA TEXT 'penalize_nl'
   @ 300,260 DRAWN EDIT oEdit10 CAPTION penalize_nl PICTURE "9" SIZE 80, 28

   @ 40, oDlg:nHeight - 56 DRAWN SIZE 100, 40 COLOR CLR_WHITE HSTYLES aStyles TEXT 'Ok' ;
      ON CLICK bOk
   @ 260, oDlg:nHeight - 56 DRAWN SIZE 100, 40 COLOR CLR_WHITE HSTYLES aStyles TEXT 'Cancel' ;
      ON CLICK {||lRes:=.F.,oDlg:Close()}

   ACTIVATE DIALOG oDlg CENTER

   IF lRes
      IF nStatus == S_INIT
         RunModule()
         IF nStatus == S_INIT
            RETURN Nil
         ENDIF
         AddEdit()
      ENDIF

      IF !Empty( sParams )
         StatusChanging( S_MODEL_PARAMS )
         ecli_RunFunc( hExt, "SetParams",{sParams} )
      ENDIF
      StatusChanging( S_MODEL_LOADING )
      ecli_RunFunc( hExt, "OpenModel",{cCurrModel}, .T. )
   ENDIF

   RETURN Nil

STATIC FUNCTION Ask()

   LOCAL o

   IF nStatus < S_MODEL_LOADED
      RETURN Nil
   ELSEIF nStatus == S_GETTOKEN
      Stop()
   ELSE
      IF Empty( HWindow():Getmain():oEdit1:GetText() )
         hwg_msgStop( "Write the question!" )
      ELSE
         StatusChanging( S_ASKING )
         ecli_RunFunc( hExt, "Ask",{HWindow():Getmain():oEdit1:GetText()}, .T. )
         o := HWindow():Getmain():oEdit2
         IF o:nTextLen > 1
            o:InsText( { hb_utf8Len(o:aText[o:nTextLen])+1, o:nTextLen}, Chr(13)+Chr(10) )
         ENDIF
         hwg_SetFocus( o:handle )
      ENDIF
   ENDIF

   RETURN Nil

STATIC FUNCTION Reset()

   ecli_RunFunc( hExt, "CloseContext",{} )
   StatusChanging( S_CNT_CREATING )
   ecli_RunFunc( hExt, "CreateContext",{}, .T. )

   RETURN Nil

STATIC FUNCTION Stop()

   StatusChanging( S_CNT_CREATED )

   RETURN Nil

STATIC FUNCTION CloseModel()

   StatusChanging( S_MODULE_STARTED )
   ecli_RunFunc( hExt, "CloseContext",{} )
   ecli_RunProc( hExt, "CloseModel",{} )

   RETURN Nil

STATIC FUNCTION TimerFunc()

   LOCAL sAns, o
   STATIC lRun := .F.

   IF !lRun
      lRun := .T.
      IF nStatus == S_MODEL_LOADING
         IF !Empty( sAns := ecli_CheckAnswer( hExt ) )
            sAns := _DropQuotes( sAns )
            IF sAns == "ok"
               StatusChanging( S_CNT_CREATING )
               ecli_RunFunc( hExt, "CreateContext",{}, .T. )
            ELSE
               StatusChanging( S_MODULE_STARTED )
            ENDIF
         ENDIF

      ELSEIF nStatus == S_CNT_CREATING
         IF !Empty( sAns := ecli_CheckAnswer( hExt ) )
            sAns := _DropQuotes( sAns )
            IF sAns == "ok"
               StatusChanging( S_CNT_CREATED )
            ELSE
               StatusChanging( S_MODULE_STARTED )
            ENDIF
            HWindow():Getmain():oEdit2:SetText( "" )
         ENDIF
      ELSEIF nStatus == S_ASKING
         IF !Empty( sAns := ecli_CheckAnswer( hExt ) )
            StatusChanging( S_GETTOKEN )
            ecli_RunFunc( hExt, "GetNextToken",{2}, .T. )
         ENDIF
      ELSEIF nStatus == S_GETTOKEN
        IF ( sAns := ecli_CheckAnswer( hExt ) ) != Nil
            sAns := _DropQuotes( sAns )
            IF Right( sAns,4 ) == '===='
               StatusChanging( S_CNT_CREATED )
               sAns := hb_strShrink( sAns, 4 ) + " =="
            ELSE
               ecli_RunFunc( hExt, "GetNextToken",{2}, .T. )
            ENDIF
            o := HWindow():Getmain():oEdit2
            o:InsText( { hb_utf8Len(o:aText[o:nTextLen])+1, o:nTextLen}, sAns )
         ENDIF
      ENDIF
      lRun := .F.
   ENDIF

   RETURN Nil

STATIC FUNCTION StatusChanging( n )

   LOCAL oMain := HWindow():Getmain()

   nStatus := n
   IF n == S_INIT
      oMain:SetTitle( "Llama.prg" )
      oMain:oBoa:oBtnAsk:lHide := .T.
      oMain:oBoa:oBtnAsk:SetText( "" )
      oMain:oBoa:oBtnReset:lHide := .T.
      oMain:oBoa:oBtnReset:SetText( "" )

   ELSEIF n == S_MODULE_STARTED
      oMain:SetTitle( "Llama.prg - connected" )
      oMain:oBoa:oBtnMod:SetText( "Load model" )
      oMain:oBoa:oBtnAsk:lHide := .T.
      oMain:oBoa:oBtnAsk:SetText( "" )
      oMain:oBoa:oBtnReset:lHide := .T.
      oMain:oBoa:oBtnReset:SetText( "" )

   ELSEIF n == S_MODEL_LOADING
      oMain:SetTitle( "Loading model..." )
      oMain:oBoa:oBtnMod:SetText( "Wait" )

   ELSEIF n == S_CNT_CREATED
      oMain:SetTitle( hb_fnameNameExt( cCurrModel ) )
      oMain:oBoa:oBtnMod:SetText( "Close" + Chr(10) + "model" )
      oMain:oBoa:oBtnAsk:lHide := .F.
      oMain:oBoa:oBtnAsk:SetText( "Ask" )
      oMain:oBoa:oBtnReset:lHide := .F.
      oMain:oBoa:oBtnReset:SetText( "Reset" )
      //gWritelog( hExt, Time() + " - model loaded" )

   ELSEIF n == S_CNT_CREATING
      oMain:SetTitle( "Context creating" )

   ELSEIF n == S_ASKING
      oMain:SetTitle( hb_fnameNameExt( cCurrModel ) + " - asking" )
      oMain:oBoa:oBtnReset:lHide := .T.
      oMain:oBoa:oBtnReset:SetText( "" )

   ELSEIF n == S_GETTOKEN
      oMain:SetTitle( hb_fnameNameExt( cCurrModel ) + " - wait for answer" )
      oMain:oBoa:oBtnAsk:SetText( "Stop" )

   ENDIF

   RETURN Nil

STATIC FUNCTION _DropQuotes( s )

   //LOCAL nPos

   //IF Left( s,1 ) == '"'
   //   s := Substr( s, 2, Len( s ) - 2 )
   //ENDIF
   //IF ( nPos := At( '\n', s ) ) > 0
   //   s := Iif( nPos==1, "", Left( s,nPos-1 ) ) + Chr(13) + Chr(10) + Substr( s,nPos+2 )
   //ENDIF
   IF Chr(10)+Chr(10) $ s
      s := StrTran( s, Chr(10)+Chr(10), Chr(10) )
   ENDIF

   RETURN s

STATIC FUNCTION IniRead( cFileName )

   LOCAL cText := Memoread( cFileName ), aText, i, s, nPos, s1, s2

   IF Empty( cText )
      RETURN Nil
   ENDIF

   aText := hb_aTokens( cText, Chr(10) )

   FOR i := 1 TO Len( aText )
      s := Iif( Left( aText[i],1 ) == ' ', Ltrim( aText[i] ), aText[i] )
      IF Left( s, 1 ) $ ";#"
         LOOP
      ENDIF
      s := Trim( Iif( Right(s,1)==Chr(13), Left( s,Len(s)-1 ), s ) )
      IF Empty( s )
         LOOP
      ENDIF
      IF ( nPos := At( '=', s ) ) > 0
         s1 := Trim( Left(s,nPos-1) )
         s2 := Ltrim( Substr( s,nPos+1 ) )
         IF Left( s1, 5 ) == "model"
            AAdd( aModels, { s2, "", "", "", "" } )
         ELSEIF s1 == "c" .OR. s1 == "n" .OR. s1 == "temp" .OR. s1 == "repeat-penalty" ;
            .OR. s1 == "top-k" .OR. s1 == "top-n" .OR. s1 == "n-keep"
            ATail( aModels )[2] += s1 + '=' + s2 + "~"
         ELSEIF s1 == "mod-type"
            ATail( aModels )[3] := s2
         ELSEIF s1 == "img-path"
            IF !( Right( s2,1 ) $ "\/" )
               s2 += hb_ps()
            ENDIF
            IF Empty( aModels )
               cImgPath := s2
            ELSE
               ATail( aModels )[4] := s2
            ENDIF
         ELSEIF s1 == "img-prefix"
            IF Empty( aModels )
               cImgPrefix := s2
            ELSE
               ATail( aModels )[5] := s2
            ENDIF
         ELSEIF s1 == "log"
            nLogLevel := Val( s2 )
         ELSEIF s1 == "font"
            oFontMain := HFont():LoadFromStr( s2 )
         ELSEIF s1 == "interval"
            IF Val( s2 ) > 0
               nInterval := Val( s2 )
            ENDIF
         ENDIF
      ENDIF
   NEXT

   IF Empty( oFontMain )
      oFontMain := HFont():Add( "Georgia", 0, 18 )
   ENDIF

   RETURN Nil