# llama.prg
Harbour bindings to llama.cpp and whisper.cpp

1. [Quick review](#quick-review)
2. [Compiling library](#compiling-library)
3. [Compiling samples](#compiling-samples)
4. [Functions list](#functions-list)
5. [Model parameters](#model-parameters)
6. [Links](#links)

## Quick review

The main goal of llama.prg project is to provide possibility to create Harbour applications, which
 can interact with local LLM - large language models.
The project provides a llama, ggml and whisper libraries, which may be linked to your application.
Under Windows it demands 64-bit MSVC compiler, under Linux/Unix - the standard 64-bit GNU C.

The project was started in 2024 and was presented at [Gitflic](https://gitflic.ru/project/alkresin/llama_prg).
Due to significunt changes in llama.cpp I rewrote the bindings code and posted it on Githib. So,
 this is a next version of llama.prg, which supports the newest (July,19 2025) llama.cpp and (July,28 2025) whisper.cpp.

## Compiling library

A preferred method to build the llama library and samples is HwBuilder - my utility, which builds programs, written on Harbour. An appropriate project file, llamalib.hwprj, is provided.
Llamalib.hwprj and other hwprj files supposes, that there is a section for 64-bit MSVC compiler in your copy of hwbuild.ini, you need to tune it:

```
[C_COMPILER_6]
id=msvc64
family=msvc
...
```

If you prefer to not use special utilities, you can build this library with following script:

#### Windows

```powershell
@echo off
if not exist lib md lib
if not exist obj md obj
if not exist obj\msvc64 md obj\msvc64
if not exist obj\whisper md obj\whisper
if not exist obj\whisper\msvc64 md obj\whisper\msvc64

call "c:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" amd64

set C_FL=/nologo /W3 /WX- /diagnostics:column /O2 /Ob2 /D _MBCS /D WIN32 /D _WINDOWS /D NDEBUG /D _CRT_SECURE_NO_WARNINGS /D GGML_SCHED_MAX_COPIES=4 /D _XOPEN_SOURCE=600 /Gm- /EHsc /MD /GS /fp:precise /Zc:wchar_t /Zc:forScope /Zc:inline /GR /std:c11 /external:W3 /Gd /TC  /utf-8 /bigobj -Illama.cpp\include /D GGML_VERSION=\"0.9.7\" /D GGML_COMMIT=\"b6c83aad5\"
set C_FL2=/nologo /W3 /WX- /diagnostics:column /O2 /Ob2 /D _MBCS /D WIN32 /D _WINDOWS /D NDEBUG /D _CRT_SECURE_NO_WARNINGS /D GGML_SCHED_MAX_COPIES=4 /D _XOPEN_SOURCE=600 /Gm- /EHsc /MD /GS /fp:precise /Zc:wchar_t /Zc:forScope /Zc:inline /GR /std:c11 /external:W3 /Gd /TC /utf-8 /bigobj /arch:AVX2 /openmp /D GGML_USE_OPENMP /D GGML_USE_LLAMAFILE /D GGML_USE_CPU_AARCH64 /D GGML_AVX2 /D GGML_FMA /D GGML_F16C -Illama.cpp\include -Illama.cpp\ggml-cpu
set CPP_FL=/nologo /W3 /WX- /diagnostics:column /O2 /Ob2 /D _MBCS /D WIN32 /D _WINDOWS /D NDEBUG /D _CRT_SECURE_NO_WARNINGS /D GGML_USE_CPU /Gm- /EHsc /MD /GS /fp:precise /Zc:wchar_t /Zc:forScope /Zc:inline /GR /std:c++17 /external:W3 /Gd /TP  /utf-8 /bigobj -Illama.cpp\include -Illama.cpp\common
set CPP_FL2=/nologo /W3 /WX- /diagnostics:column /O2 /Ob2 /D _MBCS /D WIN32 /D _WINDOWS /D NDEBUG /D _CRT_SECURE_NO_WARNINGS /Gm- /EHsc /MD /GS /fp:precise /Zc:wchar_t /Zc:forScope /Zc:inline /GR /std:c++17 /external:W3 /Gd /TP /utf-8 /bigobj /openmp /arch:AVX2 /D GGML_SCHED_MAX_COPIES=4 /D _XOPEN_SOURCE=600 /D GGML_USE_OPENMP /D GGML_USE_LLAMAFILE /D GGML_USE_CPU_AARCH64 /D GGML_AVX2 /D GGML_FMA /D GGML_F16C -Illama.cpp\include -Illama.cpp\common -Illama.cpp\ggml-cpu

set FLAG=/TP /W3 /nologo /c
set OBJ=obj\msvc64

cl.exe %FLAG% %C_FL% /I. /Fo%OBJ%\ggml.obj llama.cpp\ggml.c
cl.exe %FLAG% %C_FL% /I. /Fo%OBJ%\ggml-alloc.obj llama.cpp\ggml-alloc.c
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\ggml-backend.obj llama.cpp\ggml-backend.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\ggml-backend-dl.obj llama.cpp\ggml-backend-dl.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\ggml-backend-reg.obj llama.cpp\ggml-backend-reg.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\ggml-opt.obj llama.cpp\ggml-opt.cpp
cl.exe %FLAG% %C_FL% /I. /Fo%OBJ%\ggml-quants.obj llama.cpp\ggml-quants.c
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\ggml-threading.obj llama.cpp\ggml-threading.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\gguf.obj llama.cpp\gguf.cpp
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\ggml-cpu2.obj llama.cpp\ggml-cpu\ggml-cpu2.cpp
cl.exe %FLAG% %C_FL2% /I. /Fo%OBJ%\ggml-cpu.obj llama.cpp\ggml-cpu\ggml-cpu.c
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\hbm.obj llama.cpp\ggml-cpu\hbm.cpp
cl.exe %FLAG% %C_FL2% /I. /Fo%OBJ%\quants.obj llama.cpp\ggml-cpu\quants.c
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\traits.obj llama.cpp\ggml-cpu\traits.cpp
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\ops.obj llama.cpp\ggml-cpu\ops.cpp
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\vec.obj llama.cpp\ggml-cpu\vec.cpp
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\binary-ops.obj llama.cpp\ggml-cpu\binary-ops.cpp
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\unary-ops.obj llama.cpp\ggml-cpu\unary-ops.cpp
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\repack.obj llama.cpp\ggml-cpu\repack.cpp
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\amx.obj llama.cpp\ggml-cpu\amx\amx.cpp
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\mmq.obj llama.cpp\ggml-cpu\amx\mmq.cpp
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\cpu-feats.obj llama.cpp\ggml-cpu\arch\x86\cpu-feats.cpp
cl.exe %FLAG% %C_FL2% /I. /Fo%OBJ%\quants_arch.obj llama.cpp\ggml-cpu\arch\x86\quants_arch.c
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\repack_arch.obj llama.cpp\ggml-cpu\arch\x86\repack_arch.cpp
cl.exe %FLAG% %CPP_FL2% /I. /Fo%OBJ%\sgemm.obj llama.cpp\ggml-cpu\llamafile\sgemm.cpp
cl.exe %FLAG% %C_FL2% /Ic:\harbour\include /I. /Fo%OBJ%\hcommon.obj source\hcommon.c

lib  /out:lib\ggml.lib  %OBJ%\ggml.obj %OBJ%\ggml-alloc.obj %OBJ%\ggml-backend.obj %OBJ%\ggml-backend-dl.obj %OBJ%\ggml-backend-reg.obj %OBJ%\ggml-opt.obj %OBJ%\ggml-quants.obj %OBJ%\ggml-threading.obj %OBJ%\gguf.obj %OBJ%\ggml-cpu2.obj %OBJ%\ggml-cpu.obj %OBJ%\hbm.obj %OBJ%\quants.obj %OBJ%\traits.obj %OBJ%\ops.obj %OBJ%\vec.obj %OBJ%\binary-ops.obj %OBJ%\unary-ops.obj %OBJ%\repack.obj %OBJ%\amx.obj %OBJ%\mmq.obj %OBJ%\cpu-feats.obj %OBJ%\quants_arch.obj %OBJ%\repack_arch.obj %OBJ%\sgemm.obj %OBJ%\hcommon.obj

cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\cllama.obj source\cllama.cpp
cl.exe %FLAG% /Ic:\harbour\include /I. /Fo%OBJ%\hllama.obj source\hllama.c
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama.obj llama.cpp\llama.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-adapter.obj llama.cpp\llama-adapter.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-arch.obj llama.cpp\llama-arch.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-batch.obj llama.cpp\llama-batch.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-chat.obj llama.cpp\llama-chat.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-context.obj llama.cpp\llama-context.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-grammar.obj llama.cpp\llama-grammar.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-graph.obj llama.cpp\llama-graph.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-hparams.obj llama.cpp\llama-hparams.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-impl.obj llama.cpp\llama-impl.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-io.obj llama.cpp\llama-io.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-kv-cache.obj llama.cpp\llama-kv-cache.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-kv-cache-iswa.obj llama.cpp\llama-kv-cache-iswa.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-memory.obj llama.cpp\llama-memory.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-memory-hybrid-iswa.obj llama.cpp\llama-memory-hybrid-iswa.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-mmap.obj llama.cpp\llama-mmap.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-model.obj llama.cpp\llama-model.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-model-loader.obj llama.cpp\llama-model-loader.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-quant.obj llama.cpp\llama-quant.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-sampler.obj llama.cpp\llama-sampler.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-vocab.obj llama.cpp\llama-vocab.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\unicode.obj llama.cpp\unicode.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\unicode-data.obj llama.cpp\unicode-data.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-model-saver.obj llama.cpp\llama-model-saver.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-memory-recurrent.obj llama.cpp\llama-memory-recurrent.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llama-memory-hybrid.obj llama.cpp\llama-memory-hybrid.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\afmoe.obj llama.cpp\models\afmoe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\apertus.obj llama.cpp\models\apertus.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\arcee.obj llama.cpp\models\arcee.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\arctic.obj llama.cpp\models\arctic.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\arwkv7.obj llama.cpp\models\arwkv7.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\baichuan.obj llama.cpp\models\baichuan.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\bailingmoe.obj llama.cpp\models\bailingmoe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\bailingmoe2.obj llama.cpp\models\bailingmoe2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\bert.obj llama.cpp\models\bert.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\bitnet.obj llama.cpp\models\bitnet.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\bloom.obj llama.cpp\models\bloom.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\chameleon.obj llama.cpp\models\chameleon.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\chatglm.obj llama.cpp\models\chatglm.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\codeshell.obj llama.cpp\models\codeshell.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\cogvlm.obj llama.cpp\models\cogvlm.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\cohere2-iswa.obj llama.cpp\models\cohere2-iswa.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\command-r.obj llama.cpp\models\command-r.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\dbrx.obj llama.cpp\models\dbrx.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\deci.obj llama.cpp\models\deci.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\deepseek.obj llama.cpp\models\deepseek.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\deepseek2.obj llama.cpp\models\deepseek2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\delta-net-base.obj llama.cpp\models\delta-net-base.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\dots1.obj llama.cpp\models\dots1.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\dream.obj llama.cpp\models\dream.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\ernie4-5-moe.obj llama.cpp\models\ernie4-5-moe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\ernie4-5.obj llama.cpp\models\ernie4-5.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\eurobert.obj llama.cpp\models\eurobert.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\exaone-moe.obj llama.cpp\models\exaone-moe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\exaone.obj llama.cpp\models\exaone.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\exaone4.obj llama.cpp\models\exaone4.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\falcon-h1.obj llama.cpp\models\falcon-h1.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\falcon.obj llama.cpp\models\falcon.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\gemma-embedding.obj llama.cpp\models\gemma-embedding.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\gemma.obj llama.cpp\models\gemma.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\gemma2-iswa.obj llama.cpp\models\gemma2-iswa.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\gemma3.obj llama.cpp\models\gemma3.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\gemma3n-iswa.obj llama.cpp\models\gemma3n-iswa.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\glm4-moe.obj llama.cpp\models\glm4-moe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\glm4.obj llama.cpp\models\glm4.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\gpt2.obj llama.cpp\models\gpt2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\gptneox.obj llama.cpp\models\gptneox.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\granite-hybrid.obj llama.cpp\models\granite-hybrid.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\granite.obj llama.cpp\models\granite.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\grok.obj llama.cpp\models\grok.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\grovemoe.obj llama.cpp\models\grovemoe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\hunyuan-dense.obj llama.cpp\models\hunyuan-dense.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\hunyuan-moe.obj llama.cpp\models\hunyuan-moe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\internlm2.obj llama.cpp\models\internlm2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\jais.obj llama.cpp\models\jais.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\jais2.obj llama.cpp\models\jais2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\jamba.obj llama.cpp\models\jamba.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\kimi-linear.obj llama.cpp\models\kimi-linear.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\lfm2.obj llama.cpp\models\lfm2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\llada-moe.obj llama.cpp\models\llada-moe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\llada.obj llama.cpp\models\llada.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\llama-iswa.obj llama.cpp\models\llama-iswa.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\llama2.obj llama.cpp\models\llama2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\maincoder.obj llama.cpp\models\maincoder.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\mamba-base.obj llama.cpp\models\mamba-base.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\mamba.obj llama.cpp\models\mamba.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\mimo2-iswa.obj llama.cpp\models\mimo2-iswa.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\minicpm3.obj llama.cpp\models\minicpm3.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\minimax-m2.obj llama.cpp\models\minimax-m2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\mistral3.obj llama.cpp\models\mistral3.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\modern-bert.obj llama.cpp\models\modern-bert.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\mpt.obj llama.cpp\models\mpt.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\nemotron-h.obj llama.cpp\models\nemotron-h.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\nemotron.obj llama.cpp\models\nemotron.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\neo-bert.obj llama.cpp\models\neo-bert.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\olmo.obj llama.cpp\models\olmo.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\olmo2.obj llama.cpp\models\olmo2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\olmoe.obj llama.cpp\models\olmoe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\openai-moe-iswa.obj llama.cpp\models\openai-moe-iswa.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\openelm.obj llama.cpp\models\openelm.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\orion.obj llama.cpp\models\orion.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\paddleocr.obj llama.cpp\models\paddleocr.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\pangu-embedded.obj llama.cpp\models\pangu-embedded.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\phi2.obj llama.cpp\models\phi2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\phi3.obj llama.cpp\models\phi3.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\plamo.obj llama.cpp\models\plamo.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\plamo2.obj llama.cpp\models\plamo2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\plamo3.obj llama.cpp\models\plamo3.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\plm.obj llama.cpp\models\plm.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\qwen.obj llama.cpp\models\qwen.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\qwen2.obj llama.cpp\models\qwen2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\qwen2moe.obj llama.cpp\models\qwen2moe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\qwen2vl.obj llama.cpp\models\qwen2vl.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\qwen3.obj llama.cpp\models\qwen3.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\qwen35.obj llama.cpp\models\qwen35.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\qwen35moe.obj llama.cpp\models\qwen35moe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\qwen3moe.obj llama.cpp\models\qwen3moe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\qwen3next.obj llama.cpp\models\qwen3next.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\qwen3vl-moe.obj llama.cpp\models\qwen3vl-moe.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\qwen3vl.obj llama.cpp\models\qwen3vl.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\refact.obj llama.cpp\models\refact.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\rnd1.obj llama.cpp\models\rnd1.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\rwkv6-base.obj llama.cpp\models\rwkv6-base.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\rwkv6.obj llama.cpp\models\rwkv6.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\rwkv6qwen2.obj llama.cpp\models\rwkv6qwen2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\rwkv7-base.obj llama.cpp\models\rwkv7-base.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\rwkv7.obj llama.cpp\models\rwkv7.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\seed-oss.obj llama.cpp\models\seed-oss.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\smallthinker.obj llama.cpp\models\smallthinker.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\smollm3.obj llama.cpp\models\smollm3.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\stablelm.obj llama.cpp\models\stablelm.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\starcoder.obj llama.cpp\models\starcoder.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\starcoder2.obj llama.cpp\models\starcoder2.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\step35-iswa.obj llama.cpp\models\step35-iswa.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\t5-dec.obj llama.cpp\models\t5-dec.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\t5-enc.obj llama.cpp\models\t5-enc.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\wavtokenizer-dec.obj llama.cpp\models\wavtokenizer-dec.cpp
cl.exe %FLAG% %CPP_FL% -Isrc /I. /Fo%OBJ%\xverse.obj llama.cpp\models\xverse.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\build-info.obj llama.cpp\common\build-info.cpp
cl.exe %FLAG% %CPP_FL% -Illama.cpp/include/nlohmann /I. /Fo%OBJ%\arg.obj llama.cpp\common\arg.cpp
cl.exe %FLAG% %CPP_FL% -Illama.cpp/include/nlohmann /I. /Fo%OBJ%\chat.obj llama.cpp\common\chat.cpp
cl.exe %FLAG% %CPP_FL% -Ivendor /I. /Fo%OBJ%\chat-auto-parser-generator.obj llama.cpp\common\chat-auto-parser-generator.cpp
cl.exe %FLAG% %CPP_FL% -Ivendor /I. /Fo%OBJ%\chat-auto-parser-helpers.obj llama.cpp\common\chat-auto-parser-helpers.cpp
cl.exe %FLAG% %CPP_FL% -Ivendor /I. /Fo%OBJ%\chat-diff-analyzer.obj llama.cpp\common\chat-diff-analyzer.cpp
cl.exe %FLAG% %CPP_FL% -Ivendor /I. /Fo%OBJ%\chat-peg-parser.obj llama.cpp\common\chat-peg-parser.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\common.obj llama.cpp\common\common.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\console.obj llama.cpp\common\console.cpp
cl.exe %FLAG% %CPP_FL% -Illama.cpp/include/nlohmann /I. /Fo%OBJ%\json-schema-to-grammar.obj llama.cpp\common\json-schema-to-grammar.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\llguidance.obj llama.cpp\common\llguidance.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\log.obj llama.cpp\common\log.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\license.obj llama.cpp\common\license.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\ngram-cache.obj llama.cpp\common\ngram-cache.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\ngram-map.obj llama.cpp\common\ngram-map.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\ngram-mod.obj llama.cpp\common\ngram-mod.cpp
cl.exe %FLAG% %CPP_FL% -Ivendor /I. /Fo%OBJ%\peg-parser.obj llama.cpp\common\peg-parser.cpp
cl.exe %FLAG% %CPP_FL% -Ivendor /I. /Fo%OBJ%\preset.obj llama.cpp\common\preset.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\sampling.obj llama.cpp\common\sampling.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\speculative.obj llama.cpp\common\speculative.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\json-partial.obj llama.cpp\common\json-partial.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\reasoning-budget.obj llama.cpp\common\reasoning-budget.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\regex-partial.obj llama.cpp\common\regex-partial.cpp
cl.exe %FLAG% %CPP_FL% /I. /Fo%OBJ%\unicode2.obj llama.cpp\common\unicode2.cpp
cl.exe %FLAG% %CPP_FL% -Ivendor /I. /Fo%OBJ%\caps.obj llama.cpp\common\jinja\caps.cpp
cl.exe %FLAG% %CPP_FL% -Ivendor /I. /Fo%OBJ%\lexer.obj llama.cpp\common\jinja\lexer.cpp
cl.exe %FLAG% %CPP_FL% -Ivendor /I. /Fo%OBJ%\parser.obj llama.cpp\common\jinja\parser.cpp
cl.exe %FLAG% %CPP_FL% -Ivendor /I. /Fo%OBJ%\runtime.obj llama.cpp\common\jinja\runtime.cpp
cl.exe %FLAG% %CPP_FL% -Ivendor /I. /Fo%OBJ%\string.obj llama.cpp\common\jinja\string.cpp
cl.exe %FLAG% %CPP_FL% -Ivendor /I. /Fo%OBJ%\value.obj llama.cpp\common\jinja\value.cpp

lib  /out:lib\llama.lib  %OBJ%\cllama.obj %OBJ%\hllama.obj %OBJ%\llama.obj %OBJ%\llama-adapter.obj %OBJ%\llama-arch.obj %OBJ%\llama-batch.obj %OBJ%\llama-chat.obj %OBJ%\llama-context.obj %OBJ%\llama-grammar.obj %OBJ%\llama-graph.obj %OBJ%\llama-hparams.obj %OBJ%\llama-impl.obj %OBJ%\llama-io.obj %OBJ%\llama-kv-cache.obj %OBJ%\llama-kv-cache-iswa.obj %OBJ%\llama-memory.obj %OBJ%\llama-memory-hybrid-iswa.obj %OBJ%\llama-mmap.obj %OBJ%\llama-model.obj %OBJ%\llama-model-loader.obj %OBJ%\llama-quant.obj %OBJ%\llama-sampler.obj %OBJ%\llama-vocab.obj %OBJ%\unicode.obj %OBJ%\unicode-data.obj %OBJ%\llama-model-saver.obj %OBJ%\llama-memory-recurrent.obj %OBJ%\llama-memory-hybrid.obj %OBJ%\afmoe.obj %OBJ%\apertus.obj %OBJ%\arcee.obj %OBJ%\arctic.obj %OBJ%\arwkv7.obj %OBJ%\baichuan.obj %OBJ%\bailingmoe.obj %OBJ%\bailingmoe2.obj %OBJ%\bert.obj %OBJ%\bitnet.obj %OBJ%\bloom.obj %OBJ%\chameleon.obj %OBJ%\chatglm.obj %OBJ%\codeshell.obj %OBJ%\cogvlm.obj %OBJ%\cohere2-iswa.obj %OBJ%\command-r.obj %OBJ%\dbrx.obj %OBJ%\deci.obj %OBJ%\deepseek.obj %OBJ%\deepseek2.obj %OBJ%\delta-net-base.obj %OBJ%\dots1.obj %OBJ%\dream.obj %OBJ%\ernie4-5-moe.obj %OBJ%\ernie4-5.obj %OBJ%\eurobert.obj %OBJ%\exaone-moe.obj %OBJ%\exaone.obj %OBJ%\exaone4.obj %OBJ%\falcon-h1.obj %OBJ%\falcon.obj %OBJ%\gemma-embedding.obj %OBJ%\gemma.obj %OBJ%\gemma2-iswa.obj %OBJ%\gemma3.obj %OBJ%\gemma3n-iswa.obj %OBJ%\glm4-moe.obj %OBJ%\glm4.obj %OBJ%\gpt2.obj %OBJ%\gptneox.obj %OBJ%\granite-hybrid.obj %OBJ%\granite.obj %OBJ%\grok.obj %OBJ%\grovemoe.obj %OBJ%\hunyuan-dense.obj %OBJ%\hunyuan-moe.obj %OBJ%\internlm2.obj %OBJ%\jais.obj %OBJ%\jais2.obj %OBJ%\jamba.obj %OBJ%\kimi-linear.obj %OBJ%\lfm2.obj %OBJ%\llada-moe.obj %OBJ%\llada.obj %OBJ%\llama-iswa.obj %OBJ%\llama2.obj %OBJ%\maincoder.obj %OBJ%\mamba-base.obj %OBJ%\mamba.obj %OBJ%\mimo2-iswa.obj %OBJ%\minicpm3.obj %OBJ%\minimax-m2.obj %OBJ%\mistral3.obj %OBJ%\modern-bert.obj %OBJ%\mpt.obj %OBJ%\nemotron-h.obj %OBJ%\nemotron.obj %OBJ%\neo-bert.obj %OBJ%\olmo.obj %OBJ%\olmo2.obj %OBJ%\olmoe.obj %OBJ%\openai-moe-iswa.obj %OBJ%\openelm.obj %OBJ%\orion.obj %OBJ%\paddleocr.obj %OBJ%\pangu-embedded.obj %OBJ%\phi2.obj %OBJ%\phi3.obj %OBJ%\plamo.obj %OBJ%\plamo2.obj %OBJ%\plamo3.obj %OBJ%\plm.obj %OBJ%\qwen.obj %OBJ%\qwen2.obj %OBJ%\qwen2moe.obj %OBJ%\qwen2vl.obj %OBJ%\qwen3.obj %OBJ%\qwen35.obj %OBJ%\qwen35moe.obj %OBJ%\qwen3moe.obj %OBJ%\qwen3next.obj %OBJ%\qwen3vl-moe.obj %OBJ%\qwen3vl.obj %OBJ%\refact.obj %OBJ%\rnd1.obj %OBJ%\rwkv6-base.obj %OBJ%\rwkv6.obj %OBJ%\rwkv6qwen2.obj %OBJ%\rwkv7-base.obj %OBJ%\rwkv7.obj %OBJ%\seed-oss.obj %OBJ%\smallthinker.obj %OBJ%\smollm3.obj %OBJ%\stablelm.obj %OBJ%\starcoder.obj %OBJ%\starcoder2.obj %OBJ%\step35-iswa.obj %OBJ%\t5-dec.obj %OBJ%\t5-enc.obj %OBJ%\wavtokenizer-dec.obj %OBJ%\xverse.obj %OBJ%\build-info.obj %OBJ%\arg.obj %OBJ%\chat.obj %OBJ%\chat-auto-parser-generator.obj %OBJ%\chat-auto-parser-helpers.obj %OBJ%\chat-diff-analyzer.obj %OBJ%\chat-peg-parser.obj %OBJ%\common.obj %OBJ%\console.obj %OBJ%\json-schema-to-grammar.obj %OBJ%\llguidance.obj %OBJ%\log.obj %OBJ%\license.obj %OBJ%\ngram-cache.obj %OBJ%\ngram-map.obj %OBJ%\ngram-mod.obj %OBJ%\peg-parser.obj %OBJ%\preset.obj %OBJ%\sampling.obj %OBJ%\speculative.obj %OBJ%\json-partial.obj %OBJ%\reasoning-budget.obj %OBJ%\regex-partial.obj %OBJ%\unicode2.obj %OBJ%\caps.obj %OBJ%\lexer.obj %OBJ%\parser.obj %OBJ%\runtime.obj %OBJ%\string.obj %OBJ%\value.obj

set FLAG=/TP /W3 /nologo /EHsc /c /MD -Iwhisper -Illama.cpp\include /I. -DLOG_DISABLE_LOGS=1 /D WHISPER_VERSION=\"1.7.6\"
set OBJ=obj\whisper\msvc64

cl.exe  %FLAG% /Fo%OBJ%\whisper.obj whisper\whisper.cpp
cl.exe %FLAG% /Fo%OBJ%\common.obj whisper\common.cpp
cl.exe %FLAG% /Fo%OBJ%\grammar-parser.obj whisper\grammar-parser.cpp
cl.exe %FLAG% /Fo%OBJ%\common-ggml.obj whisper\common-ggml.cpp
cl.exe %FLAG% /Fo%OBJ%\common-whisper.obj whisper\common-whisper.cpp
cl.exe %FLAG% /Ic:\harbour\include /Fo%OBJ%\hwhisper.obj source\hwhisper.cpp

lib  /out:lib\whisper.lib  %OBJ%\whisper.obj %OBJ%\common.obj %OBJ%\grammar-parser.obj %OBJ%\common-ggml.obj %OBJ%\common-whisper.obj %OBJ%\hwhisper.obj
```
Of course, you need to use your paths.

#### Linux

```bash
#!/bin/bash
if ! [ -e lib ]; then
   mkdir lib
   chmod a+w+r+x lib
fi
if ! [ -e obj ]; then
   mkdir obj
   chmod a+w+r+x obj
fi
if ! [ -e obj/ggml ]; then
   mkdir obj/ggml
   chmod a+w+r+x obj/ggml
fi
if ! [ -e obj/ggml/gcc ]; then
   mkdir obj/ggml/gcc
   chmod a+w+r+x obj/ggml/gcc
fi
if ! [ -e obj/llama ]; then
   mkdir obj/llama
   chmod a+w+r+x obj/llama
fi
if ! [ -e obj/llama/gcc ]; then
   mkdir obj/llama/gcc
   chmod a+w+r+x obj/llama/gcc
fi

if ! [ -e obj/whisper ]; then
   mkdir obj/whisper
   chmod a+w+r+x obj/whisper
fi
if ! [ -e obj/whisper/gcc ]; then
   mkdir obj/whisper/gcc
   chmod a+w+r+x obj/whisper/gcc
fi

# Set your Harbour path here
export HRB_DIR=/home/guest/apps/harbour
export OBJ=obj/ggml/gcc

export C_FL="-DGGML_USE_CPU -DGGML_SCHED_MAX_COPIES=4 -D_GNU_SOURCE -D_XOPEN_SOURCE=600 -Dggml_base_EXPORTS -Iinclude -Iggml\src -Iggml\include -Iggml\src\ggml-cpu -Icommon -O3 -DNDEBUG -fPIC -Wshadow -Wstrict-prototypes -Wpointer-arith -Wmissing-prototypes -Werror=implicit-int -Werror=implicit-function-declaration -Wall -Wextra -Wpedantic -Wcast-qual -Wno-unused-function -Wdouble-promotion -std=gnu11 -Illama.cpp/include -Illama.cpp/ggml/src -Illama.cpp/ggml/include -Illama.cpp/ggml/src/ggml-cpu -DGGML_VERSION=\"0.0.5939\" -DGGML_COMMIT=\"f0d4d176\""
export C_FL2="-DGGML_BACKEND_BUILD -DGGML_BACKEND_SHARED -DGGML_SCHED_MAX_COPIES=4 -DGGML_SHARED -DGGML_USE_CPU_AARCH64 -DGGML_USE_LLAMAFILE -D_GNU_SOURCE -D_XOPEN_SOURCE=600 -Dggml_cpu_EXPORTS -O3 -DNDEBUG -fPIC -Wshadow -Wstrict-prototypes -Wpointer-arith -Wmissing-prototypes -Werror=implicit-int -Werror=implicit-function-declaration -Wall -Wextra -Wpedantic -Wcast-qual -Wno-unused-function -Wdouble-promotion -march=native -std=gnu11 -Illama.cpp/include -Illama.cpp/ggml/src -Illama.cpp/ggml/include -Illama.cpp/ggml/src/ggml-cpu -Illama.cpp/ggml-cpu"
export CPP_FL="-DGGML_USE_CPU -DGGML_SCHED_MAX_COPIES=4 -D_GNU_SOURCE -D_XOPEN_SOURCE=600 -Dggml_base_EXPORTS -Iggml/src -Iinclude -Iggml\src -Iggml\include -Iggml\src\ggml-cpu -Icommon -O3 -DNDEBUG -fPIC -Wmissing-declarations -Wmissing-noreturn -Wall -Wextra -Wpedantic -Wcast-qual -Wno-unused-function -Wno-array-bounds -Wextra-semi -std=gnu++17 -Illama.cpp/include -Illama.cpp/ggml/src -Illama.cpp/ggml/include -Illama.cpp/ggml/src/ggml-cpu -Illama.cpp/common"
export CPP_FL2="-DGGML_BACKEND_BUILD -DGGML_BACKEND_SHARED -DGGML_SCHED_MAX_COPIES=4 -DGGML_SHARED -DGGML_USE_CPU_AARCH64 -DGGML_USE_LLAMAFILE -D_GNU_SOURCE -D_XOPEN_SOURCE=600 -Dggml_cpu_EXPORTS -O3 -DNDEBUG -fPIC -Wmissing-declarations -Wmissing-noreturn -Wall -Wextra -Wpedantic -Wcast-qual -Wno-unused-function -Wno-array-bounds -Wextra-semi -march=native -std=gnu++17 -Illama.cpp/include -Illama.cpp/ggml/src -Illama.cpp/ggml/include -Illama.cpp/ggml/src/ggml-cpu -Illama.cpp/common -Illama.cpp/ggml-cpu"

gcc -c -Wall -Wunused $C_FL -I. -o$OBJ/ggml.o llama.cpp/ggml.c
gcc -c -Wall -Wunused $C_FL -I. -o$OBJ/ggml-alloc.o llama.cpp/ggml-alloc.c
gcc -c -Wall -Wunused $CPP_FL -I. -o$OBJ/ggml-backend.o llama.cpp/ggml-backend.cpp
gcc -c -Wall -Wunused $CPP_FL -I. -o$OBJ/ggml-opt.o llama.cpp/ggml-opt.cpp
gcc -c -Wall -Wunused $CPP_FL -I. -o$OBJ/ggml-threading.o llama.cpp/ggml-threading.cpp
gcc -c -Wall -Wunused $C_FL -I. -o$OBJ/ggml-quants.o llama.cpp/ggml-quants.c
gcc -c -Wall -Wunused $CPP_FL -I. -o$OBJ/gguf.o llama.cpp/gguf.cpp
gcc -c -Wall -Wunused $CPP_FL -I. -o$OBJ/ggml-backend-reg.o llama.cpp/ggml-backend-reg.cpp
gcc -c -Wall -Wunused $CPP_FL2 -I. -o$OBJ/ggml-cpu2.o llama.cpp/ggml-cpu/ggml-cpu2.cpp
gcc -c -Wall -Wunused $C_FL2 -I. -o$OBJ/ggml-cpu.o llama.cpp/ggml-cpu/ggml-cpu.c
gcc -c -Wall -Wunused $CPP_FL2 -I. -o$OBJ/hbm.o llama.cpp/ggml-cpu/hbm.cpp
gcc -c -Wall -Wunused $C_FL2 -I. -o$OBJ/quants.o llama.cpp/ggml-cpu/quants.c
gcc -c -Wall -Wunused $CPP_FL2 -I. -o$OBJ/traits.o llama.cpp/ggml-cpu/traits.cpp
gcc -c -Wall -Wunused $CPP_FL2 -I. -o$OBJ/ops.o llama.cpp/ggml-cpu/ops.cpp
gcc -c -Wall -Wunused $CPP_FL2 -I. -o$OBJ/vec.o llama.cpp/ggml-cpu/vec.cpp
gcc -c -Wall -Wunused $CPP_FL2 -I. -o$OBJ/binary-ops.o llama.cpp/ggml-cpu/binary-ops.cpp
gcc -c -Wall -Wunused $CPP_FL2 -I. -o$OBJ/unary-ops.o llama.cpp/ggml-cpu/unary-ops.cpp
gcc -c -Wall -Wunused $CPP_FL2 -I. -o$OBJ/amx.o llama.cpp/ggml-cpu/amx/amx.cpp
gcc -c -Wall -Wunused $CPP_FL2 -I. -o$OBJ/mmq.o llama.cpp/ggml-cpu/amx/mmq.cpp
gcc -c -Wall -Wunused $CPP_FL2 -I. -o$OBJ/cpu-feats.o llama.cpp/ggml-cpu/arch/x86/cpu-feats.cpp
gcc -c -Wall -Wunused $C_FL2 -I. -o$OBJ/quants_arch.o llama.cpp/ggml-cpu/arch/x86/quants_arch.c
gcc -c -Wall -Wunused $CPP_FL2 -I. -o$OBJ/repack.o llama.cpp/ggml-cpu/arch/x86/repack.cpp
gcc -c -Wall -Wunused $CPP_FL2 -I. -o$OBJ/sgemm.o llama.cpp/ggml-cpu/llamafile/sgemm.cpp
gcc -c -Wall -Wunused $C_FL2 -I$HRB_DIR/include -I. -o$OBJ/hcommon.o source/hcommon.c

ar rc  lib/libggml.a  $OBJ/*.o

export OBJ=obj/llama/gcc

gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/cllama.o source/cllama.cpp
gcc -c -Wall -Wunused  -I$HRB_DIR/include -I. -o$OBJ/hllama.o source/hllama.c
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama.o llama.cpp/llama.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-adapter.o llama.cpp/llama-adapter.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-arch.o llama.cpp/llama-arch.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-batch.o llama.cpp/llama-batch.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-chat.o llama.cpp/llama-chat.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-context.o llama.cpp/llama-context.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-grammar.o llama.cpp/llama-grammar.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-graph.o llama.cpp/llama-graph.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-hparams.o llama.cpp/llama-hparams.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-impl.o llama.cpp/llama-impl.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-io.o llama.cpp/llama-io.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-kv-cache-iswa.o llama.cpp/llama-kv-cache-iswa.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-kv-cache.o llama.cpp/llama-kv-cache.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-memory.o llama.cpp/llama-memory.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-mmap.o llama.cpp/llama-mmap.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-model.o llama.cpp/llama-model.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-model-loader.o llama.cpp/llama-model-loader.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-quant.o llama.cpp/llama-quant.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-sampling.o llama.cpp/llama-sampling.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-vocab.o llama.cpp/llama-vocab.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/unicode.o llama.cpp/unicode.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/unicode-data.o llama.cpp/unicode-data.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-model-saver.o llama.cpp/llama-model-saver.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-memory-recurrent.o llama.cpp/llama-memory-recurrent.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llama-memory-hybrid.o llama.cpp/llama-memory-hybrid.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/build-info.o llama.cpp/common/build-info.cpp
gcc -c -Wall -Wunused  $CPP_FL -Illama.cpp/include/nlohmann -I. -o$OBJ/arg.o llama.cpp/common/arg.cpp
gcc -c -Wall -Wunused  $CPP_FL -Illama.cpp/include/nlohmann -I. -o$OBJ/chat.o llama.cpp/common/chat.cpp
gcc -c -Wall -Wunused  $CPP_FL  -I. -o$OBJ/common.o llama.cpp/common/common.cpp
gcc -c -Wall -Wunused  $CPP_FL  -I. -o$OBJ/console.o llama.cpp/common/console.cpp
gcc -c -Wall -Wunused  $CPP_FL -Illama.cpp/include/nlohmann -I. -o$OBJ/chat-parser-xml-toolcall.o llama.cpp/common/chat-parser-xml-toolcall.cpp
gcc -c -Wall -Wunused  $CPP_FL -Illama.cpp/include/nlohmann -I. -o$OBJ/download.o llama.cpp/common/download.cpp
gcc -c -Wall -Wunused  $CPP_FL -Illama.cpp/include/nlohmann -I. -o$OBJ/json-schema-to-grammar.o llama.cpp/common/json-schema-to-grammar.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/llguidance.o llama.cpp/common/llguidance.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/log.o llama.cpp/common/log.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/ngram-cache.o llama.cpp/common/ngram-cache.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/sampling.o llama.cpp/common/sampling.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/speculative.o llama.cpp/common/speculative.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/chat-parser.o llama.cpp/common/chat-parser.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/json-partial.o llama.cpp/common/json-partial.cpp
gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/regex-partial.o llama.cpp/common/regex-partial.cpp

for file in llama.cpp/models/*.cpp; do
  gcc -c -Wall -Wunused  $CPP_FL -I. -o$OBJ/$(basename "$file" .cpp).o llama.cpp/models/$(basename "$file" .cpp).cpp
done

ar rc lib/libllama.a  $OBJ/*.o

export OBJ=obj/whisper/gcc
export FLAG="-xc++ -DWHISPER_VERSION=\"1.7.6\" -c -Wall -std=c++11 -fPIC -O3 -pthread  -march=native -mtune=native -Wno-array-bounds -Wno-format-truncation -Wextra-semi -Iwhisper -Illama.cpp/include -D_XOPEN_SOURCE=600 -D_GNU_SOURCE -DNDEBUG -DLOG_DISABLE_LOGS=1 -c -I$HRB_DIR/include -I."

gcc $FLAG -o$OBJ/whisper.o whisper/whisper.cpp
gcc $FLAG -o$OBJ/common.o whisper/common.cpp
gcc $FLAG -o$OBJ/grammar-parser.o whisper/grammar-parser.cpp
gcc $FLAG -o$OBJ/common-ggml.o whisper/common-ggml.cpp
gcc $FLAG -o$OBJ/common-whisper.o whisper/common-whisper.cpp
gcc $FLAG -o$OBJ/hwhisper.o source/hwhisper.cpp

ar rc  lib/libwhisper.a  $OBJ/*.o
```

## Compiling samples

It is better to use HwBuilder to build a sample application - test.hwprj is provided, but you may use the following bat file:

#### Windows

```powershell
@echo off
rem Set your Harbour path here
set HB_PATH=c:\harbour

set HB_LIBS=gtwvt.lib hbvm.lib hbrtl.lib gtgui.lib gtwin.lib hbcpage.lib hblang.lib hbrdd.lib hbmacro.lib hbpp.lib rddntx.lib rddcdx.lib rddfpt.lib hbsix.lib hbcommon.lib hbct.lib hbcplr.lib hbpcre.lib hbzlib.lib
set LLAMA_LIBS=llama.lib ggml.lib
set VC_LIBS=ucrt.lib user32.lib gdi32.lib comdlg32.lib shell32.lib comctl32.lib winspool.lib advapi32.lib winmm.lib ws2_32.lib iphlpapi.lib OleAut32.Lib Ole32.Lib

call "c:\Program Files (x86)\Microsoft Visual Studio\2019\Community\VC\Auxiliary\Build\vcvarsall.bat" amd64

%HB_PATH%\bin\harbour -n -q -w -i%HB_PATH%\include test.prg

cl.exe /TP /W3 /nologo /c /I%HB_PATH%\include /I. /Fotest.obj test.c
link /NODEFAULTLIB:libucrt.lib /NODEFAULTLIB:msvcrt.lib /INCREMENTAL:NO /NOLOGO /SUBSYSTEM:CONSOLE /TLBID:1 /MANIFEST /MANIFESTUAC:"level='asInvoker' uiAccess='false'" /manifest:embed /DYNAMICBASE /NXCOMPAT /MACHINE:X64  /machine:x64 /LIBPATH:%HB_PATH%\lib\win\msvc64 /LIBPATH:. /LIBPATH:lib  test.obj %HB_LIBS% %LLAMA_LIBS% %VC_LIBS%
```

#### Linux
```bash
#!/bin/bash
# Set your Harbour path here
export HRB_DIR=/home/guest/apps/harbour

$HRB_DIR/bin/linux/gcc/harbour -n -q -i$HRB_DIR/include test.prg
gcc -c -Wall -I$HRB_DIR/include -otest.o test1.c
gcc  test.o -otest -L$HRB_DIR/lib/linux/gcc -Llib -Wl,--start-group -lgttrm -lhbvm -lhbrtl -lgtcgi -lgttrm -lhbcpage -lhblang -lhbrdd -lhbmacro -lhbpp -lrddntx -lrddcdx -lrddfpt -lhbsix -lhbcommon -lhbct -lhbcplr -lllama -lggml -lpthread -lm -lz -lpcre -ldl -Wl,--end-group -fPIC -O3 -Wall -lstdc++ -shared-libgcc
```

## Functions list

#### llm_open_model( cModelName[, cParameters] ) --> nSuccess
The function opens AI model **cModel** with a set of parameters (**cParameters**),
which is a string with name value pairs, divided by a space:

name1 value1 name2 value2...

See the list of possible parameters in appropriate section of this manual.

Returns value **nSuccess** is 0 if the function has completed successfully.

#### llm_create_context() --> nSuccess
The function creates the dialog context.

Returns value **nSuccess** is 0 if the function has completed successfully.

#### llm_init_prompt( cPrompt )

#### llm_ask( cQuestion )

#### llm_getnexttoken() --> xResult

#### llm_close_model()
Closes previously opened model.

#### llm_print_timings()

Prints some time parameters of a dialog.

#### llm_rediron( n, cFile ) --> handle

Redirects output ( **n** = 1 - stdout, **n** = 2 - stderr ) to a file **cFile**, returns file handle.

#### llm_rediroff( n, handle )

Cancel output redirection ( **n** = 1 - stdout, **n** = 2 - stderr ), **handle** - a file handle, returned by llm_rediron().

#### llm_whisper_print_usage() --> cList

Returns the list of parameters and it's current values.

#### llm_whisper_set_params( cParams ) --> nSuccess
The function sets the model parameters. Parameters list **cParams** is a string
 with name=value pairs, divided by ~ character:

name1=value1~name2=value2~...

Return value **nSuccess** is 0 if the function has completed successfully.

#### llm_whisper_open_model( cModel ) --> nSuccess
The function opens AI model **cModel**.

Return value **nSuccess** is 0 if the function has completed successfully.

#### llm_whisper_close_model()
Closes previously opened model.

#### llm_whisper_recognize( cWavFile, [@cStringOut] ) --> nSuccess
Starts the process of recognizing wav file. **cWavFile** - a file name.

Return value **nSuccess** is 0 if the function has completed successfully.

#### llm_whisper_setcallback( cCallbackName )
Sets callback function with a name **cCallbackName**, which may output recognizing results

#### llm_whisper_abort()
Aborts the process of recognizing

#### llm_whisper_print_timings()

## Model parameters

   Below is a list of parameters, which may be used currently in Llama.prg. I use the same
abbreviations, as in a main llama.cpp example. The description is borrowed from llama.cpp/examples/main/README.md.

 - -c  - (default: 4096, 0 = loaded from model) This is a **--ctx-size** option, which
   allows you to set the size of the prompt context used by the LLaMA models during text
   generation. A larger context size helps the model to better comprehend and generate
   responses for longer input or conversations.

 - -n  - (default: -1) This is a **--n-predict** option, which controls the number of tokens the model
   generates in response to the input prompt. By adjusting this value, you can influence
   the length of the generated text. A higher value will result in longer text, while
   a lower value will produce shorter text.

   A value of -1 will enable infinite text generation, even though we have a finite context
   window. When the context window is full, some of the earlier tokens
   (half of the tokens after **--n-keep**) will be discarded. The context must then
   be re-evaluated before generation can resume. On large models and/or large context windows,
   this will result in significant pause in output.

 - -temp - (default: 0.8) Temperature is a
   hyperparameter that controls the randomness of the generated text. It affects the
   probability distribution of the model's output tokens. A higher temperature (e.g., 1.5)
   makes the output more random and creative, while a lower temperature (e.g., 0.5) makes
   the output more focused, deterministic, and conservative. The default value is 0.8,
   which provides a balance between randomness and determinism. At the extreme, a
   temperature of 0 will always pick the most likely next token, leading to identical
   outputs in each run.

 - --repeat-penalty - (default: 1.0) Control the repetition of token sequences in the generated text.
   The **repeat-penalty** option helps prevent the model from generating repetitive or monotonous
   text. A higher value (e.g., 1.5) will penalize repetitions more strongly, while a lower value
   (e.g., 0.9) will be more lenient.

 - --repeat-last-n N`: Last n tokens to consider for penalizing repetition
   (default: 64, 0 = disabled, -1 = ctx-size).

 - --top-k - (default: 40) Limit the next token selection to the K most probable tokens.
   Top-k sampling is a text generation method that selects the next token only from the top k
   most likely tokens predicted by the model. It helps reduce the risk of generating
   low-probability or nonsensical tokens, but it may also limit the diversity of the output.
   A higher value for top-k (e.g., 100) will consider more tokens and lead to more diverse text,
   while a lower value (e.g., 10) will focus on the most probable tokens and generate more
   conservative text.

 - --top-p - (default: 0.95) Limit the next token selection to a subset of tokens with a cumulative probability above a threshold P.
   Top-p sampling, also known as nucleus sampling, is another text generation method that selects
   the next token from a subset of tokens that together have a cumulative probability of
   at least p. This method provides a balance between diversity and quality by considering
   both the probabilities of tokens and the number of tokens to sample from. A higher value
   for top-p (e.g., 0.95) will lead to more diverse text, while a lower value (e.g., 0.5)
   will generate more focused and conservative text.

 - --keep option allows users to retain the original prompt when the model
   runs out of context, ensuring a connection to the initial instruction or conversation topic
   is maintained. It is the number of tokens from the initial prompt to retain when the model
   resets its internal context. By default, this value is set to 0 (meaning no tokens are kept).
   Use `-1` to retain all tokens from the initial prompt.

 - -t - **--threads N**: Set the number of threads to use during generation. For optimal
   performance, it is recommended to set this value to the number of physical CPU cores
   your system has (as opposed to the logical number of cores). Using the correct number
   of threads can greatly improve performance.

 - -tb N, --threads-batch N: Set the number of threads to use during batch and prompt processing.
   In some systems, it is beneficial to use a higher number of threads during batch processing
   than during generation. If not specified, the number of threads used for batch processing
   will be the same as the number of threads used for generation.

 - --min-p - (default: 0.05) Sets a minimum base probability threshold for token selection.
   The Min-P sampling method was designed as an alternative to Top-P, and aims to ensure a
   balance of quality and variety. The parameter *p* represents the minimum probability for
   a token to be considered, relative to the probability of the most likely token. For example,
   with *p*=0.05 and the most likely token having a probability of 0.9, logits with a value
   less than 0.045 are filtered out.

 - --typical N: Enable locally typical sampling with parameter p (default: 1.0, 1.0 = disabled).
   Locally typical sampling promotes the generation of contextually coherent and diverse text
   by sampling tokens that are typical or expected based on the surrounding context.
   By setting the parameter p between 0 and 1, you can control the balance between producing
   text that is locally coherent and diverse. A value closer to 1 will promote more contextually
   coherent tokens, while a value closer to 0 will promote more diverse tokens. A value equal
   to 1 disables locally typical sampling.

 Mirostat Sampling. Mirostat is an algorithm that actively maintains the quality of
   generated text within a desired range during text generation. It aims to strike a
   balance between coherence and diversity, avoiding low-quality output caused by
   excessive repetition (boredom traps) or incoherence (confusion traps).

 - --mirostat N: Enable Mirostat sampling, controlling perplexity during text generation
   (default: 0, 0 = disabled, 1 = Mirostat, 2 = Mirostat 2.0).

 - --mirostat-lr N: Set the Mirostat learning rate, parameter eta (default: 0.1).
   Option sets the Mirostat learning rate (eta). The learning rate influences how
   quickly the algorithm responds to feedback from the generated text. A lower learning
   rate will result in slower adjustments, while a higher learning rate will make the
   algorithm more responsive. The default value is `0.1.

 - --mirostat-ent N: Set the Mirostat target entropy, parameter tau (default: 5.0).
   Option sets the Mirostat target entropy (tau), which represents the desired perplexity
   value for the generated text. Adjusting the target entropy allows you to control the
   balance between coherence and diversity in the generated text. A lower value will
   result in more focused and coherent text, while a higher value will lead to more
   diverse and potentially less coherent text. The default value is `5.0.

 Example usage: --mirostat 2 --mirostat-lr 0.05 --mirostat-ent 3.0

 DRY (Don't Repeat Yourself) sampling is an effective technique for reducing repetition in
   generated text even across long contexts by penalizing tokens based on their recent usage
   patterns (original [PR link](https://github.com/oobabooga/text-generation-webui/pull/5677)).

 - --dry-multiplier N: Set the DRY sampling multiplier (default: 0.0, 0.0 = disabled).
   Option controls the strength of the DRY sampling effect. A value of 0.0 disables DRY
   sampling, while higher values increase its influence. A typical recommended value is 0.8.

 - --dry-base N: Set the DRY sampling base value (default: 1.75).
   Option sets the base value for the exponential penalty calculation in DRY sampling.
   Higher values lead to more aggressive penalization of repetitions.

 - --dry-allowed-length N: Set the allowed length for DRY sampling (default: 2).
   Option sets the maximum length of repeated sequences that will not be penalized.
   Repetitions shorter than or equal to this length are not penalized, allowing for
   natural repetitions of short phrases or common words.

 - --dry-penalty-last-n N: Set DRY penalty for the last n tokens (default: -1, 0 = disable, -1 = context size).
   Option controls how many recent tokens to consider when applying the DRY penalty.
   A value of -1 considers the entire context. Use a positive value to limit the
   consideration to a specific number of recent tokens.

 - --dry-sequence-breaker STRING: Add a sequence breaker for DRY sampling. Can be used more than once to add multiple sequence breakers. Using this clears out the default breakers, which consist of: `['\n', ':', '"', '*']`.
   If the string `"none"` is supplied, no sequence breakers are used.
   Option adds a single sequence breaker and can be used more than once to specify
   multiple sequence breakers. Sequence breakers interrupt sequence matching and break
   the input into parts where matching can be applied.

 DRY sampling provides more nuanced control over text generation, particularly for reducing long-range repetitions and maintaining global coherence.

 Example usage: --dry-multiplier 0.8 --dry-base 1.75 --dry-allowed-length 2 --dry-penalty-last-n -1 --dry-sequence-breaker "тАФ" --dry-sequence-breaker "##"

## Links

[Project web page](http://www.kresin.ru/en/llama_prg.html)

[llama.cpp](https://github.com/ggml-org/llama.cpp)

[whisper.cpp](https://github.com/ggml-org/whisper.cpp)

[HwBuilder](http://www.kresin.ru/en/hwbuilder.html)

[Ext](https://gitflic.ru/project/alkresin/ext)