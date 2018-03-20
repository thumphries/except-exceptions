# except-exceptions

Exception-related functions for `ExceptT`.

As of `exceptions-0.9.0`, `ExceptT` has a valid and sensible
`MonadMask` instance. If available, this package simply re-exports the
relevant functions from `exceptions`.
