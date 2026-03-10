/*
 *
 * Llama.prg - Harbour wrappers for llama.cpp,
 * which allows chatting with local AI models
 *
 * audiorecord.c - few wrappers for miniaudio
 *
 * This work is based on llama.cpp and whisper projects
 * https://github.com/ggerganov/llama.cpp
 * https://github.com/ggerganov/whisper.cpp
 *
 * Copyright 2024 Alexander S.Kresin <alex@kresin.ru>
 * www - http://www.kresin.ru
*/

#include "miniaudio.h"
#include "hbapi.h"
#include "hbvm.h"
#include "hbstack.h"
#include "hbapiitm.h"

typedef struct
{
   void          *pCoder;
   //PHB_DYNS pSym_onEvent;
   short int    bPlaying;
} udata;

// Callback function, called when receiving data from the microphone
void data_capture_callback( ma_device* pDevice, void* pOutput, const void* pInput, ma_uint32 frameCount )
{
   udata *pUData = (udata*)pDevice->pUserData;
   if( pUData == NULL ) return;
   ma_encoder* pEncoder = (ma_encoder*)(pUData->pCoder);
   if( pEncoder == NULL ) return;

    // Write the received data to the encoder (which saves it to a file)
    ma_encoder_write_pcm_frames( pEncoder, pInput, frameCount, NULL );

    (void)pOutput;
}

/* ma_device_capture_init( cFile, nSampleRate, nChannels ) -> pDevice
 */
HB_FUNC( MA_DEVICE_CAPTURE_INIT ) {

   const char* filename = HB_ISCHAR(1)? hb_parc( 1 ) : "out.wav";
   ma_uint32 sampleRate = HB_ISNUM(2)? hb_parni( 2 ) : 44100;
   ma_uint32 channels = HB_ISNUM(3)? hb_parni( 3 ) : 1;
   ma_result result;
   ma_device_config deviceConfig;
   ma_device * pDevice;
   ma_encoder_config encoderConfig;
   ma_encoder *pEncoder;
   udata   * pUData;

   // Configuring the encoder to save to a WAV file
   encoderConfig = ma_encoder_config_init( ma_encoding_format_wav, ma_format_s16, channels, sampleRate );
   pEncoder = (ma_encoder *) hb_xgrab( sizeof(ma_encoder) );
   result = ma_encoder_init_file(filename, &encoderConfig, pEncoder);
   if (result != MA_SUCCESS) {
      hb_xfree( pEncoder );
      hb_ret();
      return;
   }

   pUData = (udata *) hb_xgrab( sizeof(udata) );
   memset( pUData, 0, sizeof(udata) );
   pUData->pCoder = (void*)pEncoder;

   // Configuring the capture device
   deviceConfig = ma_device_config_init( ma_device_type_capture );
   deviceConfig.capture.format   = ma_format_s16;
   deviceConfig.capture.channels = channels;
   deviceConfig.sampleRate       = sampleRate;
   deviceConfig.pUserData        = pUData;
   deviceConfig.dataCallback     = data_capture_callback;

   // Initializing the device
   pDevice = (ma_device *) hb_xgrab( sizeof(ma_device) );
   result = ma_device_init( NULL, &deviceConfig, pDevice );
   if (result != MA_SUCCESS) {
      hb_xfree( pDevice );
      ma_encoder_uninit( pEncoder );
      hb_xfree( pEncoder );
      hb_xfree( pUData );
      hb_ret();
      return;
   }
   hb_retptr( (void*) pDevice );
}

/* ma_device_capture_uninit( pDevice ) -> Nil
 */
HB_FUNC( MA_DEVICE_CAPTURE_UNINIT ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );
   udata *pUData = (udata*)pDevice->pUserData;

   ma_encoder_uninit( (ma_encoder *)(pUData->pCoder) );
   ma_device_uninit( pDevice );
   hb_xfree( (ma_encoder *)(pUData->pCoder) );
   hb_xfree( pUData );
   hb_xfree( pDevice );

}

/* ma_device_start( pDevice ) -> nResult
 */
HB_FUNC( MA_DEVICE_START ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );

   ((udata*)pDevice->pUserData)->bPlaying = 1;
   hb_retni( ma_device_start( pDevice ) );
}

/* ma_device_stop( pDevice ) -> nResult
 */
HB_FUNC( MA_DEVICE_STOP ) {

   ma_device * pDevice = (ma_device*) hb_parptr( 1 );

   ((udata*)pDevice->pUserData)->bPlaying = 0;
   hb_retni( ma_device_stop( pDevice ) );
}

/* ma_sleep( nMilliseconds ) -> Nil
 */
HB_FUNC( MA_SLEEP ) {

   ma_sleep( hb_parni(1) );
}