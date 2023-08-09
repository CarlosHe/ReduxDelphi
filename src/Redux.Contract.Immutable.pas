unit Redux.Contract.Immutable;

interface

uses
  System.Generics.Collections,
  Redux.Callback;

type

  IImmutableList<T> = interface
    ['{86D27495-BB81-400D-9DC9-C005DE523B3E}']
    function Insert(const AIndex: Integer; const AItem: T): IImmutableList<T>;
    function Filter(const AFilterCallback: TFilterCallback<T>): IImmutableList<T>;
    function Map(const AMapperCallback: TMapperCallback<T>): IImmutableList<T>;
    function Count: Integer;
    function Items(const AIndex: Integer): T;
    function GetEnumerator: TEnumerator<T>;
  end;

implementation

end.
