# aide.el

An Emacs front end for GPT APIs like OpenAI.

  > Note: This is still alpha software. Use with caution.

## Usage

Prerequisite:

``` emacs-lisp
(setq openai-api-key "<api-key>")
```

Then you can select any region and run `M-x aide-openai-completion-region-insert`.

You can also run `M-x aide-openai-completion-buffer-insert`, which grabs the current buffer as a string, send it to OpenAI API and insert the result at the end of the buffer. This is like the [OpenAI playground](https://beta.openai.com/playground) where you can run the command multiple times to continue the conversion in the same buffer.

  > Note: **This command reads th ENTIRE buffer**.

## License

See aide.el.
