unit Redux.Immutable;

interface

uses
  System.Generics.Collections,
  Redux.Contract.Immutable,
  Redux.Callback;

type

  TImmutableList<T> = class(TInterfacedObject, IImmutableList<T>)
  strict private
    { strict private declarations }
    constructor Create(const AImmutableList: IImmutableList<T> = nil);
  private
    { private declarations }
    FList: TList<T>;
  protected
    { protected declarations }
  public
    { public declarations }
    destructor Destroy; override;
    function Insert(const AIndex: Integer; const AItem: T): IImmutableList<T>;
    function Filter(const AFilterCallback: TFilterCallback<T>): IImmutableList<T>;
    function Map(const AMapperCallback: TMapperCallback<T>): IImmutableList<T>;
    function Count: Integer;
    function Items(const AIndex: Integer): T;
    function GetEnumerator: TEnumerator<T>;
    class function New(const AImmutableList: IImmutableList<T> = nil): IImmutableList<T>; overload;
  end;

implementation

{ TImmutableList<T> }

function TImmutableList<T>.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TImmutableList<T>.Create(const AImmutableList: IImmutableList<T>);
var
  LItem: T;
begin
  FList := TList<T>.Create;
  if AImmutableList <> nil then
    for LItem in AImmutableList do
      FList.Add(LItem);
end;

destructor TImmutableList<T>.Destroy;
begin
  FList.Free;
  inherited;
end;

function TImmutableList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := FList.GetEnumerator;
end;

function TImmutableList<T>.Insert(const AIndex: Integer; const AItem: T): IImmutableList<T>;
var
  LNewList: TImmutableList<T>;
begin
  LNewList := TImmutableList<T>.Create(Self);
  TImmutableList<T>(LNewList).FList.Insert(AIndex, AItem);
  Result := LNewList;
end;

function TImmutableList<T>.Items(const AIndex: Integer): T;
begin
  Result := FList.Items[AIndex];
end;

function TImmutableList<T>.Filter(const AFilterCallback: TFilterCallback<T>): IImmutableList<T>;
var
  LItem: T;
  LNewList: TImmutableList<T>;
begin
  LNewList := TImmutableList<T>.Create();
  for LItem in FList do
  begin
    if AFilterCallback(LItem) then
      TImmutableList<T>(LNewList).FList.Add(LItem)
  end;
  Result := LNewList;
end;

function TImmutableList<T>.Map(const AMapperCallback: TMapperCallback<T>): IImmutableList<T>;
var
  LItem: T;
  LNewList: TImmutableList<T>;
begin
  LNewList := TImmutableList<T>.Create();
  for LItem in FList do
  begin
    TImmutableList<T>(LNewList).FList.Add(AMapperCallback(LItem))
  end;
  Result := LNewList;
end;

class function TImmutableList<T>.New(const AImmutableList: IImmutableList<T>): IImmutableList<T>;
begin
  Result := Self.Create(AImmutableList);
end;

end.
