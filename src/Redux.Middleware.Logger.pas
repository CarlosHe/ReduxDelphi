unit Redux.Middleware.Logger;

interface

uses
  Redux.Store,
  Redux.Action,
  Redux.Callback,
  Redux.Contract.Action;

function LoggerMiddleware(const ADispatcher: TDispatcherCallback<IAction>): TDispatcherCallback<IAction>;

implementation

uses
  System.SysUtils,
  System.TypInfo,
  System.Rtti,
  Windows;

function GetRttiFromInterface(const AInterface: IInterface): string;
var
  LObject: TObject;
  LContext: TRttiContext;
begin
  LObject := AInterface as TObject;
  Result := LContext.GetType(LObject.ClassType).ToString;
end;

function LoggerMiddleware(const ADispatcher: TDispatcherCallback<IAction>): TDispatcherCallback<IAction>;
begin
  Result := function(const AAction: IAction): IAction
    begin
      OutputDebugString(PWideChar(GetRttiFromInterface(AAction)));
      Result := ADispatcher(AAction);
    end;
end;

end.
