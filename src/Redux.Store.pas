unit Redux.Store;

interface

uses
  System.SysUtils,
  System.Generics.Collections,
  Redux.Callback,
  Redux.Contract.Store;

type

  TStore<TState, TRdxAction> = class(TInterfacedObject, IStore<TState, TRdxAction>)
  strict private
    { strict private declarations }
    constructor Create(const AReducer: TReducerCallback<TState, TRdxAction>; const AState: TState);
  private
    { private declarations }
    FState: TState;
    FReducer: TReducerCallback<TState, TRdxAction>;
    FSubscriber: TList<TSubscriberCallback<TState>>;
    FMiddlewares: TList<TMiddlewareCallback<TRdxAction>>;
  protected
    { protected declarations }
    function InnerDispatch(const AAction: TRdxAction): TRdxAction;
    function ApplyMiddleware: TDispatcherCallback<TRdxAction>;
  public
    { public declarations }
    destructor Destroy; override;
    procedure Subscribe(const ASubscriber: TSubscriberCallback<TState>);
    procedure Unsubscribe(const ASubscriber: TSubscriberCallback<TState>);
    procedure Dispatch(const AAction: TRdxAction);
    procedure AddMiddleware(const AMiddleware: TMiddlewareCallback<TRdxAction>);
    function GetState: TState;
    class function New(const AReducer: TReducerCallback<TState, TRdxAction>; const AState: TState): IStore<TState, TRdxAction>;
  end;

implementation

{ TStore<TState, TRdxAction> }

function TStore<TState, TRdxAction>.ApplyMiddleware: TDispatcherCallback<TRdxAction>;
var
  LDispatcher: TDispatcherCallback<TRdxAction>;
  LMiddleware: TMiddlewareCallback<TRdxAction>;
begin
  LDispatcher := InnerDispatch;
  for LMiddleware in FMiddlewares do
    LDispatcher := LMiddleware(LDispatcher);
  Result := LDispatcher;
end;

function TStore<TState, TRdxAction>.InnerDispatch(const AAction: TRdxAction): TRdxAction;
var
  LSubscriber: TSubscriberCallback<TState>;
begin
  FState := FReducer(FState, AAction);
  for LSubscriber in FSubscriber do
    LSubscriber(FState);
end;

class function TStore<TState, TRdxAction>.New(const AReducer: TReducerCallback<TState, TRdxAction>; const AState: TState): IStore<TState, TRdxAction>;
begin
  Result := Self.Create(AReducer, AState);
end;

constructor TStore<TState, TRdxAction>.Create(const AReducer: TReducerCallback<TState, TRdxAction>; const AState: TState);
begin
  FState := AState;
  FReducer := AReducer;
  FSubscriber := TList < TSubscriberCallback < TState >>.Create();
  FMiddlewares := TList < TMiddlewareCallback < TRdxAction >>.Create();
end;

procedure TStore<TState, TRdxAction>.AddMiddleware(const AMiddleware: TMiddlewareCallback<TRdxAction>);
begin
  FMiddlewares.Add(AMiddleware);
end;

destructor TStore<TState, TRdxAction>.Destroy;
begin
  FSubscriber.Clear;
  FMiddlewares.Clear;
  FSubscriber.Free;
  FMiddlewares.Free;
  inherited;
end;

procedure TStore<TState, TRdxAction>.Dispatch(const AAction: TRdxAction);
begin
  ApplyMiddleware()(AAction);
end;

function TStore<TState, TRdxAction>.GetState: TState;
begin
  Result := FState;
end;

procedure TStore<TState, TRdxAction>.Subscribe(const ASubscriber: TSubscriberCallback<TState>);
begin
  FSubscriber.Add(ASubscriber);
end;

procedure TStore<TState, TRdxAction>.Unsubscribe(const ASubscriber: TSubscriberCallback<TState>);
begin
  FSubscriber.Remove(ASubscriber);
end;

end.
