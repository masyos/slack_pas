unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Slack.Components;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    LabeledEditUrl: TLabeledEdit;
    LabeledEditText: TLabeledEdit;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LabeledEditTextChange(Sender: TObject);
  private
    FSlack: TSlackWebHookComponent;

    procedure LoadConf;
    procedure SaveConf;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  JsonConf;

const
  ConfFilename = 'local.settings.json';
  ConfWebHookUrlKey: utf8string = '/Slack/IncomingWebhook';


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSlack := TSlackWebHookComponent.Create(self);
  LoadConf;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FSlack.WebHookUrl:=LabeledEditUrl.Text;
  Memo1.Text := FSlack.PostForTextOnly(LabeledEditText.Text);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SaveConf;
  FSlack.Free;
end;

procedure TForm1.LabeledEditTextChange(Sender: TObject);
begin

end;

procedure TForm1.LoadConf;
var
  cfg: TJSONConfig;
begin
  cfg := TJSONConfig.Create(nil);
  try
    cfg.Filename := ConfFilename;
    LabeledEditUrl.Text := cfg.GetValue(ConfWebHookUrlKey, LabeledEditUrl.Text);
  finally
    cfg.Free;
  end;
end;

procedure TForm1.SaveConf;
var
  cfg: TJSONConfig;
begin
  cfg := TJSONConfig.Create(nil);
  try
    cfg.Filename := ConfFilename;

    cfg.SetValue(ConfWebHookUrlKey, LabeledEditUrl.Text);
  finally
    cfg.Free;
  end;
end;

end.

