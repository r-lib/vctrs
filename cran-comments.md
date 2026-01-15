This is a rather large minor release.

We expect a few breakages as listed here https://github.com/r-lib/vctrs/issues/2131. All packages have been given at least a month to update.

We are not yet fully API compliant, but we have made significant progress on this.

From:

```
    Found non-API calls to R: ‘ATTRIB’, ‘BODY’, ‘CLOENV’, ‘ENCLOS’,
      ‘EXTPTR_PROT’, ‘EXTPTR_TAG’, ‘FORMALS’, ‘FRAME’, ‘HASHTAB’,
      ‘IS_S4_OBJECT’, ‘LEVELS’, ‘OBJECT’, ‘PRENV’, ‘PRVALUE’,
      ‘R_PromiseExpr’, ‘Rf_allocSExp’, ‘Rf_findVarInFrame3’, ‘SETLENGTH’,
      ‘SET_ATTRIB’, ‘SET_BODY’, ‘SET_CLOENV’, ‘SET_ENCLOS’,
      ‘SET_FORMALS’, ‘SET_GROWABLE_BIT’, ‘SET_OBJECT’, ‘SET_S4_OBJECT’,
      ‘SET_TRUELENGTH’, ‘STDVEC_DATAPTR’, ‘STRING_PTR’, ‘TRUELENGTH’,
      ‘UNSET_S4_OBJECT’
  These entry points may be removed soon:
  ‘SET_ENCLOS’, ‘STDVEC_DATAPTR’, ‘SET_S4_OBJECT’, ‘UNSET_S4_OBJECT’, ‘EXTPTR_PROT’, ‘EXTPTR_TAG’, ‘FRAME’, ‘HASHTAB’, ‘IS_S4_OBJECT’, ‘BODY’, ‘FORMALS’, ‘CLOENV’, ‘ENCLOS’, ‘OBJECT’, ‘SET_FORMALS’, ‘SET_BODY’, ‘SET_CLOENV’, ‘STRING_PTR’, ‘LEVELS’, ‘SET_GROWABLE_BIT’, ‘TRUELENGTH’, ‘SET_TRUELENGTH’, ‘SETLEN
```

To:

```
    Found non-API calls to R: ‘ATTRIB’, ‘PRVALUE’, ‘Rf_findVarInFrame3’,
      ‘SET_ATTRIB’
```

- Removing `PRVALUE()` and `Rf_findVarInFrame3()` require changes to base R as discussed with Luke and outlined in https://gist.github.com/lionel-/1ebcbd5ec69c0775d514c329522408a3.

- Both `ATTRIB()` and `SET_ATTRIB()` are newly generating a NOTE, and we have not yet had time to unwind our usage of them. The latest release of rlang is in the same state, and base R's replacement APIs for these seem to still be a little in flux.

We request that you release vctrs anyways since we have made significant progress here, which will unblock a dplyr release. In follow up rlang and vctrs releases we can focus on removing `ATTRIB()` and `SET_ATTRIB()` as well!
