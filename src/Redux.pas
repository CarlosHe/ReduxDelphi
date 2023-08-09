unit Redux;

interface

uses

  Redux.Contract.Action,
  Redux.Contract.Immutable,
  Redux.Contract.Store,
  Redux.Action,
  Redux.Immutable,
  Redux.Store;

type

  TActionInit = Redux.Action.TActionInit;
  TActionSetText = Redux.Action.TActionSetText;
  TActionSetBool = Redux.Action.TActionSetBool;
  TActionSetGUID = Redux.Action.TActionSetGUID;

  TImmutableList<T> = class(Redux.Immutable.TImmutableList<T>);

  TStore<TState, TRdxAction> = class(Redux.Store.TStore<TState, TRdxAction>);

implementation

end.
