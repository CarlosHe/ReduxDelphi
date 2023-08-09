unit DemoCounterForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Redux.Callback,
  Redux.Contract.Store,
  Redux.Store;

type
  TEnumAction = (INIT, INCREMENT, DECREMENT);

  TFormDemoCounter = class(TForm)
    ButtonInc: TButton;
    ButtonDec: TButton;
    LabelCounter: TLabel;
    procedure ButtonIncClick(Sender: TObject);
    procedure ButtonDecClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FStore: IStore<Integer, TEnumAction>;
  end;

var
  FormDemoCounter: TFormDemoCounter;

implementation

{$R *.dfm}


procedure TFormDemoCounter.ButtonIncClick(Sender: TObject);
begin
  FStore.dispatch(INCREMENT);
end;

procedure TFormDemoCounter.ButtonDecClick(Sender: TObject);
begin
  FStore.dispatch(DECREMENT);
end;

procedure TFormDemoCounter.FormShow(Sender: TObject);
var
  LReducer: TReducerCallback<Integer, TEnumAction>;
begin
  LReducer := function(const AState: Integer; const AAction: TEnumAction): Integer
    begin
      case AAction of
        INCREMENT:
          Result := AState + 1;
        DECREMENT:
          Result := AState - 1;
      else
        Result := AState;
      end;
    end;

  FStore := TStore<Integer, TEnumAction>.New(LReducer, 0);

  FStore.Subscribe(
    procedure(const AState: Integer)
    begin
      LabelCounter.Caption := IntToStr(AState);
    end);

  FStore.dispatch(INIT);
end;

end.
