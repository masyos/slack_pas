(* SPDX-License-Identifier: Apache-2.0 *)
(*
   SlackPas(SlackAPI for Pascal) version 0.0.1
   Copyright 2022 YOSHIDA, Masahiro.
 *)
unit Slack.IncomingWebhook;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
{$IFnDEF FPC}
{$ELSE}
{$ENDIF}
  ;

function SlackIncomingWebhook(const Url, Body: utf8string; var Response: utf8string): Integer;
function SlackIncomingWebhookText(const Url, Text: utf8string; var Response: utf8string): Integer;


implementation

uses
{$IFnDEF FPC}
{$ELSE}
  fpjson, fphttpclient, opensslsockets;
{$ENDIF}

function SlackBuildPlaneText(const Text: utf8string): utf8string;
var
  obj: TJSONObject;
begin
  Result := EmptyStr;
  obj := TJSONObject.Create;
  try
    obj.Add('text', Text);
	Result := obj.AsJSON;
  finally
    obj.Free;
  end;
end;

function SlackIncomingWebhook(const Url, Body: utf8string; var Response: utf8string
  ): Integer;
var
  Client: TFPHttpClient;
  Stream: TStringStream;
begin
  Result := 0;
  Stream := TStringStream.Create;
  try
    Client := TFPHttpClient.Create(nil);
    try
      Client.AddHeader('Content-Type','application/json; charset=UTF-8');
      Client.KeepConnection := false;
      Client.RequestBody := TRawByteStringStream.Create(Body);
      try
        Client.Post(Url, Stream);
        Result := Client.ResponseStatusCode;
        Response := Stream.DataString;
      finally
        Client.RequestBody.Free;
      end;
    finally
      Client.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function SlackIncomingWebhookText(const Url, Text: utf8string;
  var Response: utf8string): Integer;
begin
  Result := SlackIncomingWebhook(Url, SlackBuildPlaneText(Text), Response);
end;

end.

