unit TodoActions;

interface

uses
  Redux.Contract.Action,
  Redux.Action,
  TodoStates;

type
  TAddTodoAction = class(TActionSetText);

  TDeleteTodoAction = class(TActionSetGUID);

  TCompleteTodoAction = class(TActionSetGUID);

  TCompleteAllTodosAction = class(TActionSetBool);

  TClearCompletedTodosAction = class(TActionInit);

  TFilterTodosAction = class(TInterfacedObject, IAction)
  strict private
    { strict private declarations }
    constructor Create(const AFilter: TTodosFilter); overload;
  private
    FFilter: TTodosFilter;
    { private declarations }
  protected
    { protected declarations }
    function GetFilter: TTodosFilter;
    procedure SetFilter(const AFilter: TTodosFilter);
  public
    { public declarations }
    property Filter: TTodosFilter read GetFilter write SetFilter;
    class function New(const AFilter: TTodosFilter): IAction;
  end;

implementation

{ TFilterTodosAction }

constructor TFilterTodosAction.Create(const AFilter: TTodosFilter);
begin
  FFilter := AFilter;
end;

function TFilterTodosAction.GetFilter: TTodosFilter;
begin
  Result := FFilter;
end;

class function TFilterTodosAction.New(const AFilter: TTodosFilter): IAction;
begin
  Result := Self.Create(AFilter);
end;

procedure TFilterTodosAction.SetFilter(const AFilter: TTodosFilter);
begin
  FFilter := AFilter;
end;

end.
