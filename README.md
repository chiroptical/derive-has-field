Derive HasField instances
===

The [`OverloadedRecordDot`][overloaded-record-dot] syntax is surprisingly nice.
I really enjoy writing code with this extension and I was originally hesistant.

[Persistent][persistent] has a really nice feature where it will automatically remove
prefixes from models. Given a model like,

```
BankAccount
  accountNumber String
```

You would normally reference this field as `bankAccountAccountNumber`. However,
with overloaded record dot you can write `bankAccount.accountNumber` which is
much nicer.

At work, I really wanted this for **every** record. With this library, I can
write,

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

import DeriveHasField

data BankAccount =
  BankAccount
    { bankAccountAccountNumber :: String
    }

deriveHasField ''BankAccount

-- alternatively, for prefixes that don't match the type name
deriveHasFieldWith (dropPrefix "bankAccount") ''BankAccount
```

[overloaded-record-dot]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_record_dot.html
[persistent]: https://github.com/yesodweb/persistent
