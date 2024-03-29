# aide.el

An Emacs front end for GPT APIs like OpenAI.

  > Note: This is still alpha software. Use with caution.


## Demos

### Chat API
🆕 aide.el now supports [OpenAI chat API endpoint](https://platform.openai.com/docs/api-reference/chat), which uses   `gpt-3.5-turbo` model by default. Check out the demo below:


![aide-demo-2023-04-23 17 51 12](https://user-images.githubusercontent.com/2715151/233868185-def7bae5-8eb4-4150-a7c5-6e40c5ba516e.gif)


### Completion API
![aide-demo-completion-api](https://user-images.githubusercontent.com/2715151/147772615-da36b3ab-a32a-4f7f-b185-e62f3972f8b7.gif). 

## Installation

__Dependency__: aide.el depends on [emacs-request](https://github.com/tkf/emacs-request) package.

I haven't uploaded aide.el to any package repo yet (e.g., MELPA), so you have to install manually for now: 

Copy the content of `aide.el` to Emacs and evaluate it. 


When using [straight.el](https://github.com/radian-software/straight.el), you can install aide.el as follows:

```emacs-lisp
(use-package aide
  :straight (aide :type git
                  :host github
                  :repo "junjizhi/aide.el"))
```

## Usage

Prerequisite: set `aide-openai-api-key-getter` to retrieve your API key for OpenAI.

``` emacs-lisp
(setq aide-openai-api-key-getter (lambda () "<api-key>"))
```


> 💡 You can use any arbitrary means to retrieve your password, you can decrypt a local GPG file, access your favorite password-store or hardcode the secret directly in your config file.

Then you can select any region and run `M-x aide-openai-completion-region-insert`.

You can also run `M-x aide-openai-complete-buffer-insert`, which grabs the current buffer as a string, send it to OpenAI API and insert the result at the end of the buffer. This is like the [OpenAI playground](https://beta.openai.com/playground) where you can run the command multiple times to continue the conversion in the same buffer.

  > Note: **This command reads the ENTIRE buffer**.

### Custom variables

You can set the custom variables to control the aide.el behavior.

To do so, you can `M-x customize`, select `External` > `aide`.

Or you can set it directly in elisp:

``` emacs-lisp
(setq aide-max-tokens 200)
```

### Using password-store to retrieve your OpenAI key

You can configure auth-source to use [password store](https://www.passwordstore.org/) as a backend through the following function call:

```emacs-lisp
(auth-source-pass-enable)
```

Straight.el users, can load and configuration the *auth-source* package as follows:

```emacs-lisp
(use-package auth-source
  :straight (:type built-in)

  :config
  (auth-source-pass-enable))
```


Eventually, you can define the `aide-openai-api-key-getter` custom variable to retrieve the password from your password store:

```emacs-lisp
(customize-set-variable
  'aide-openai-api-key-getter
  (lambda ()
    (auth-source-pass-get 'secret "openai.com/user-handle/api-key")))
```

or again, when using straight:

```emacs-lisp
(use-package aide
  :straight (aide :type git
                  :host github
                  :repo "junjizhi/aide.el")
  :custom
  (aide-openai-api-key-getter (lambda ()
                                (auth-source-pass-get 'secret "openai.com/user-handle/api-key"))))
```

#### Why would you want to use password store?

Using password store, provides you a way to avoid having to store your API key in plaintext inside of your Emacs configuration that you may want to check into version control.

Furthermore, your password store may enforce a mechanism to ensure that secrets become unavailable after a certain time, warranting another passphrase prompt if a key retrieval is attempted.

All these benefit, you may get for free when using a password store.

## License

See aide.el.
