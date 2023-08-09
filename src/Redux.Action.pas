unit Redux.Action;

interface

uses
  Redux.Contract.Action;

type

  TActionInit = class(TInterfacedObject, IAction)
  strict private
    { strict private declarations }
    constructor Create;
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
    class function New: IAction;
  end;

  TActionSetText = class(TInterfacedObject, IAction)
  strict private
    { strict private declarations }
    constructor Create(const AText: string = '');
  private
    { private declarations }
    FText: string;
  protected
    { protected declarations }
    procedure SetText(const AText: string);
    function GetText: string;
  public
    { public declarations }
    property Text: string read GetText write SetText;
    class function New(const AText: string = ''): IAction;
  end;

  TActionSetBool = class(TInterfacedObject, IAction)
  strict private
    { strict private declarations }
    constructor Create(const ABool: Boolean = False);
  private
    { private declarations }
    FBool: Boolean;
    function GetBool: Boolean;
    procedure SetBool(const ABool: Boolean);
  protected
    { protected declarations }
  public
    { public declarations }
    property Bool: Boolean read GetBool write SetBool;
    class function New(const ABool: Boolean = False): IAction;
  end;

  TActionSetGUID = class(TInterfacedObject, IAction)
  strict private
    { strict private declarations }
    constructor Create(const AGUID: TGUID);
  private
    { private declarations }
    FGUID: TGUID;
  protected
    { protected declarations }
    function GetGUID: TGUID;
    procedure SetGUID(const AGUID: TGUID);
  public
    { public declarations }
    property GUID: TGUID read GetGUID write SetGUID;
    class function New(const AGUID: TGUID): IAction;
  end;

implementation

{ TActionSetText }

constructor TActionSetText.Create(const AText: String);
begin
  FText := AText;
end;

function TActionSetText.GetText: string;
begin
  Result := FText;
end;

class function TActionSetText.New(const AText: string): IAction;
begin
  Result := Self.Create(AText);
end;

procedure TActionSetText.SetText(const AText: string);
begin
  FText := AText;
end;

{ TActionSetBool }

constructor TActionSetBool.Create(const ABool: Boolean);
begin
  FBool := ABool;
end;

function TActionSetBool.GetBool: Boolean;
begin
  Result := FBool;
end;

class function TActionSetBool.New(const ABool: Boolean): IAction;
begin
  Result := Self.Create(ABool);
end;

procedure TActionSetBool.SetBool(const ABool: Boolean);
begin
  FBool := ABool;
end;

{ TActionSetGUID }

constructor TActionSetGUID.Create(const AGUID: TGUID);
begin
  FGUID := AGUID;
end;

function TActionSetGUID.GetGUID: TGUID;
begin
  Result := FGUID;
end;

class function TActionSetGUID.New(const AGUID: TGUID): IAction;
begin
  Result := Self.Create(AGUID);
end;

procedure TActionSetGUID.SetGUID(const AGUID: TGUID);
begin
  FGUID := AGUID;
end;

{ TActionInit }

constructor TActionInit.Create;
begin

end;

class function TActionInit.New: IAction;
begin
  Result := Self.Create;
end;

end.
