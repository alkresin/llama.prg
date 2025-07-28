# llama.prg
Harbour bindings to llama,cpp

1. [Quick review](#quick-review)
2. [Functions list](#functions-list)
3. [Model parameters](#model-parameters)
4. [Links](#links)

## Quick review

The main goal of llama.prg project is to provide possibility to create Harbour applications, which
 can interact with local LLaMA language models.
The project provides a llama, ggml and whisper libraries, which may be linked to your application.
Under Windows it demands 64-bit MSVC compiler, under Linux/Unix - the standard 64-bit GNU C.

The project was started in 2024 and was presented at [Gitflic](https://gitflic.ru/project/alkresin/llama_prg).
Due to significunt changes in llama.cpp I rewrote the bindings code and posted it on Githib. So,
 this is a next version of llama.prg, which supports the newest (July 2025) llama.cpp.

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

[HwBuilder](http://www.kresin.ru/en/hwbuilder.html)

[Ext](https://gitflic.ru/project/alkresin/ext)