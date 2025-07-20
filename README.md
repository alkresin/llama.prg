# llama.prg
Harbour bindings to llama,cpp

1. [Quick review](#quick-review)
2. [Links](#links)

## Quick review

The main goal of llama.prg project is to provide possibility to create Harbour applications, which
 can interact with local LLaMA language models.
The project provides a llama and ggml libraries, which may be linked to your application.
Under Windows it demands 64-bit MSVC compiler, under Linux/Unix - the standard 64-bit GNU C.

The project was started in 2024 and was presented at [Gitflic](https://gitflic.ru/project/alkresin/llama_prg).
Due to significunt changes in llama.cpp I rewrote the bindings code and posted it on Githib. So,
 this is a next version of llama.prg, which supports the newest (July 2025) llama.cpp.

## Links

[Project web page](http://www.kresin.ru/en/llama_prg.html)

[llama.cpp](https://github.com/ggml-org/llama.cpp)

[HwBuilder](http://www.kresin.ru/en/hwbuilder.html)

[Ext](https://gitflic.ru/project/alkresin/ext)