unit Redux.Contract.Store;

interface

uses
  Redux.Callback;

type

  IStore<TState, TRdxAction> = interface
    ['{F7134E83-7E82-4F0F-8C2E-DF1F07174845}']
    procedure Subscribe(const ASubscriber: TSubscriberCallback<TState>);
    procedure Unsubscribe(const ASubscriber: TSubscriberCallback<TState>);
    procedure Dispatch(const AAction: TRdxAction);
    procedure AddMiddleware(const AMiddleware: TMiddlewareCallback<TRdxAction>);
    function GetState(): TState;
  end;

implementation

end.
