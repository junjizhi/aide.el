# aide.el

An Emacs front end for GPT APIs like OpenAI.

  > Note: This is still alpha software. Use with caution.

## Demo

![aide.ei-demo](https://user-images.githubusercontent.com/2715151/147772615-da36b3ab-a32a-4f7f-b185-e62f3972f8b7.gif)

## Installation

__Dependency__: aide.el depends on [emacs-request](https://github.com/tkf/emacs-request) package.

I haven't uploaded aide.el to any package repo yet (e.g., MELPA), so you have to install manually for now: 

Copy the content of `aide.el` to Emacs and evaluate it. 


## Usage

Prerequisite: set `aide-openai-api-key-getter` to to retrieve your API key for OpenAI.

``` emacs-lisp
(setq aide-openai-api-key-getter (lambda () "<api-key>"))
```

Then you can select any region and run `M-x aide-openai-completion-region-insert`.

You can also run `M-x aide-openai-completion-buffer-insert`, which grabs the current buffer as a string, send it to OpenAI API and insert the result at the end of the buffer. This is like the [OpenAI playground](https://beta.openai.com/playground) where you can run the command multiple times to continue the conversion in the same buffer.

  > Note: **This command reads th ENTIRE buffer**.

### Custom variables

You can set the custom variables to control the aide.el behavior.

To do so, you can `M-x customize`, select `External` > `aide`.

Or you can set it directly in elisp:

``` emacs-lisp
(setq aide-max-tokens 200)
```

## License

See aide.el.
