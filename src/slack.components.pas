(* SPDX-License-Identifier: MIT *)
(*
   slack_pas(SlackAPI for Pascal) version 0.0.1
   Copyright 2022-2023 YOSHIDA, Masahiro.
 *)
unit Slack.Components;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  Slack.Webhook;

type

  { TSlackWebHookComponent }

  TSlackWebHookComponent = class(TComponent)
  private
    FClient: TSlackWebhookClient;
    function GetWebHookUrl: string;
    procedure SetWebHookUrl(AValue: string);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    {
      Incoming Webhook Post.
      @param AUrl Webhook URL.
      @param ABody JSON.
      @return response string.
    }
    function Post(const ABody: string): string;

    {
      Incoming Webhook Post.
      @param AUrl Webhook URL.
      @param ABody JSON.
      @param AResponse response stream.
    }
    procedure Post(const ABody: string; const AResponse: TStream);

    {
      Incoming Webhook Post. (text only)
      @param ABody plane text string.
      @return response string.
    }
    function PostForPlaneText(const ABody: string): string;

  published
    property WebHookUrl: string read GetWebHookUrl write SetWebHookUrl;
  end;

implementation

{ TSlackWebHookComponent }

function TSlackWebHookComponent.GetWebHookUrl: string;
begin
  Result := FClient.WebHookUrl;
end;

procedure TSlackWebHookComponent.SetWebHookUrl(AValue: string);
begin
  FClient.WebHookUrl := AValue;
end;

constructor TSlackWebHookComponent.Create(AOwner: TComponent);
begin
  FClient := TSlackWebhookClient.Create;
  inherited Create(AOwner);
end;

destructor TSlackWebHookComponent.Destroy;
begin
  FClient.Free;
  inherited Destroy;
end;

function TSlackWebHookComponent.Post(const ABody: string): string;
var
  s: utf8string;
begin
  Result := EmptyStr;
  s := ABody;
  if s = EmptyStr then Exit;
  Result := FClient.Post(s);
end;

procedure TSlackWebHookComponent.Post(const ABody: string;
  const AResponse: TStream);
var
  s: utf8string;
begin
  s := ABody;
  if s = EmptyStr then Exit;
  FClient.Post(s, AResponse);
end;

function TSlackWebHookComponent.PostForPlaneText(const ABody: string): string;
var
  s: utf8string;
begin
  s := ABody;
  Result := FClient.PostForPlaneText(s);
end;



end.
