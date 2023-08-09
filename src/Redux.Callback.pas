unit Redux.Callback;

interface

type

  TFilterCallback<T> = reference to function(const AItem: T): Boolean;
  TMapperCallback<T> = reference to function(const AItem: T): T;
  TSubscriberCallback<TState> = reference to procedure(const AState: TState);
  TReducerCallback<TState, TRdxAction> = reference to function(const AState: TState; const AAction: TRdxAction): TState;
  TDispatcherCallback<TRdxAction> = reference to function(const AAction: TRdxAction): TRdxAction;
  TMiddlewareCallback<TRdxAction> = reference to function(const ADispatcher: TDispatcherCallback<TRdxAction>): TDispatcherCallback<TRdxAction>;

implementation

end.
