unit TodoReducer;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  Redux.Contract.Action,
  Redux.Action,
  TodoActions,
  TodoStates;

function ApplicationReducer(const AState: IApplicationState; const AAction: IAction): IApplicationState;

implementation

function AddTodoReducer(State: ITodoList; Action: TAddTodoAction): ITodoList;
var
  AGUID: TGUID;
  ATodo: ITodo;
begin
  CreateGUID(AGUID);
  ATodo := TTodo.Create(Action.Text, False, AGUID);
  Result := State.Insert(0, ATodo);
end;

function ClearCompletedTodosReducer(State: ITodoList; Action: TClearCompletedTodosAction): ITodoList;
begin
  Result := State.Filter(
    function(const Todo: ITodo): Boolean
    begin
      Result := not Todo.IsCompleted;
    end);
end;

function CompleteAllTodosReducer(State: ITodoList; Action: TCompleteAllTodosAction): ITodoList;
begin
  Result := State.Map(
    function(const Todo: ITodo): ITodo
    begin
      Result := TTodo.Create(Todo.Text, Action.Bool, Todo.Id);
    end);
end;

function CompleteTodoReducer(State: ITodoList; Action: TCompleteTodoAction): ITodoList;
begin
  Result := State.Map(
    function(const Todo: ITodo): ITodo
    begin
      if Todo.Id = Action.GUID then
        Result := TTodo.Create(Todo.Text, not Todo.IsCompleted, Todo.Id)
      else
        Result := Todo;
    end);
end;

function DeleteTodoReducer(State: ITodoList; Action: TDeleteTodoAction): ITodoList;
begin
  Result := State.Filter(
    function(const Todo: ITodo): Boolean
    begin
      Result := Todo.Id <> Action.GUID;
    end);
end;

function TodosReducer(State: ITodoList; Action: IAction): ITodoList;
begin
  if (Action is TAddTodoAction) then
    Result := AddTodoReducer(State, TAddTodoAction(Action))

  else if (Action is TClearCompletedTodosAction) then
    Result := ClearCompletedTodosReducer(State, TClearCompletedTodosAction(Action))

  else if (Action is TCompleteAllTodosAction) then
    Result := CompleteAllTodosReducer(State, TCompleteAllTodosAction(Action))

  else if (Action is TCompleteTodoAction) then
    Result := CompleteTodoReducer(State, TCompleteTodoAction(Action))

  else if (Action is TDeleteTodoAction) then
    Result := DeleteTodoReducer(State, TDeleteTodoAction(Action))
  else
    Result := State;
end;

function FilterReducer(State: TTodosFilter; Action: IAction): TTodosFilter;
begin
  if Action is TFilterTodosAction then
    Result := (Action as TFilterTodosAction).Filter
  else
    Result := State;
end;

function ApplicationReducer(const AState: IApplicationState; const AAction: IAction): IApplicationState;
begin
  Result := TApplicationState.Create(
    TodosReducer(AState.Todos, AAction),
    FilterReducer(AState.Filter, AAction));
end;

end.
