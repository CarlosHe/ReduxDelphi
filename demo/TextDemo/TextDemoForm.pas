unit TextDemoForm;

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
  Redux.Contract.Action,
  Redux.Action;

type

  TState = record
    MyText1: string;
    MyText2: string;
  end;

  TActionSetText1 = class(TActionSetText);
  TActionSetText2 = class(TActionSetText);

  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    LabelSaved: TLabel;
    ButtonSave: TButton;
    procedure Edit1Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
  private
    FStore: IStore<TState, IAction>;
  end;

var
  Form1: TForm1;

implementation

uses
  Redux.Store;

const
  InitState: TState = (MyText1: ''; MyText2: '');

{$R *.dfm}
  { TForm1 }

procedure TForm1.ButtonSaveClick(Sender: TObject);
begin
  FStore.Dispatch(TActionSetText2.New(Edit1.Text));
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  FStore.Dispatch(TActionSetText1.New(Edit1.Text));
end;

procedure TForm1.FormShow(Sender: TObject);
var
  FReducer: TReducerCallback<TState, IAction>;
begin
  FReducer :=
      function(const AState: TState; const AAction: IAction): TState
    begin
      Result := AState;
      if AAction is TActionSetText1 then
        Result.MyText1 := TActionSetText1(AAction).Text;
      if AAction is TActionSetText2 then
        Result.MyText2 := TActionSetText2(AAction).Text;
    end;

  FStore := TStore<TState, IAction>.New(FReducer, InitState);

  FStore.Subscribe(
    procedure(const AState: TState)
    begin
      Label1.Caption := 'Typed: ' + AState.MyText1;
      LabelSaved.Caption := 'Saved: ' + AState.MyText2;
    end);

  FStore.Dispatch(TActionInit.New);
end;

end.
