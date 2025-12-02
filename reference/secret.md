# Set or get secrets from the keyring

Some services require credentials to access data. This function uses
keyring to safely store those credentials on your computer.

## Usage

``` r
set_secret(name, secret = NULL)

get_secret(name)
```

## Arguments

- name:

  Name of the secret to set or get as a character (e.g. `"nl_api_key"`).

- secret:

  Optionally a character string with the secret, alternatively the
  system will prompt the user.

## Value

`set_secret()` returns `TRUE` when a secret has successfully been set.
`get_secret()` returns the secret as a character string.

## Details

When working with a cluster it might be advantageous to use a specific
keyring, this can be done by setting the `keyring_backend` option in R.

The package uses the option `getRad.key_prefix` as a prefix to all keys
stored. If you want to use multiple keys for the same api you can
manipulate this option.
