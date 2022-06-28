(* SPDX-License-Identifier: Apache-2.0 *)
(*
   SlackPas(SlackAPI for Pascal) version 0.0.1
   Copyright 2022 YOSHIDA, Masahiro.
 *)
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, Menus,
  StdCtrls, StdActns;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    Button1: TButton;
    Button2: TButton;
    FileExit1: TFileExit;
    Label3: TLabel;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    UrlEdit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FResponse: utf8string;

    procedure SaveConf;
    procedure LoadConf;
    procedure WriteLog(err: integer);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  JsonConf,
  Slack.IncomingWebHook;

const
  ConfFilename = 'local.settings.json';
  WebHookUrlKey: utf8string = '/Slack/IncomingWebhook';

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  err: integer;
begin
  // simple text.
  if (not (URLEdit1.Text = EmptyStr)) and (not (Memo1.Lines.Text = EmptyStr)) then
  begin
    FResponse := EmptyStr;
    err := SlackIncomingWebhookText(URLEdit1.Text,
        Memo1.Lines.Text,
        FResponse);
    WriteLog(err);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  err: integer;
begin
  // block json.
  if (not (URLEdit1.Text = EmptyStr)) and (not (Memo1.Lines.Text = EmptyStr)) then
  begin
    FResponse := EmptyStr;
    err := SlackIncomingWebhook(URLEdit1.Text,
        Memo1.Lines.Text,
        FResponse);
    WriteLog(err);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FResponse := EmptyStr;
  LoadConf;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SaveConf;
end;

procedure TForm1.SaveConf;
var
  cfg: TJSONConfig;
begin
  cfg := TJSONConfig.Create(nil);
  try
    cfg.Filename := ConfFilename;

    cfg.SetValue(WebHookUrlKey, URLEdit1.Text);
  finally
    cfg.Free;
  end;
end;

procedure TForm1.LoadConf;
var
  cfg: TJSONConfig;
begin
  cfg := TJSONConfig.Create(nil);
  try
    cfg.Filename := ConfFilename;
    URLEdit1.Text := cfg.GetValue(WebHookUrlKey, URLEdit1.Text);
  finally
    cfg.Free;
  end;
end;

procedure TForm1.WriteLog(err: integer);
begin
  Memo2.Lines.Clear;
  Memo2.Lines.Add('Error Code: ' + IntToStr(err));
  Memo2.Lines.Add(FResponse);
end;

end.

