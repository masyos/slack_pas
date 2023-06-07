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

    procedure Post(const ABody: utf8string);

    procedure SaveConf;
    procedure LoadConf;
    procedure WriteLog;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  JsonConf,
  Slack.WebHook;

const
  ConfFilename = 'local.settings.json';
  ConfWebHookUrlKey: utf8string = '/Slack/IncomingWebhook';

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  body: utf8string;
begin
  // simple text.
  if (not (URLEdit1.Text = EmptyStr)) and (not (Memo1.Lines.Text = EmptyStr)) then
  begin
    body := SlackBuildPlaneTextBody(Memo1.Lines.Text);
    Post(body);
    WriteLog;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  body: utf8string;
begin
  // block json.
  if (not (URLEdit1.Text = EmptyStr)) and (not (Memo1.Lines.Text = EmptyStr)) then
  begin
    body := Memo1.Lines.Text;
    Post(body);
    WriteLog;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoadConf;
  FResponse := EmptyStr;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  SaveConf;
end;

procedure TForm1.Post(const ABody: utf8string);
begin
  TSlackWebhookClient.SimplePost(URLEdit1.Text, ABody);
end;

procedure TForm1.SaveConf;
var
  cfg: TJSONConfig;
begin
  cfg := TJSONConfig.Create(nil);
  try
    cfg.Filename := ConfFilename;

    cfg.SetValue(ConfWebHookUrlKey, URLEdit1.Text);
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
    URLEdit1.Text := cfg.GetValue(ConfWebHookUrlKey, URLEdit1.Text);
  finally
    cfg.Free;
  end;
end;

procedure TForm1.WriteLog;
begin
  Memo2.Lines.Clear;
  Memo2.Lines.Add(FResponse);
end;

end.

