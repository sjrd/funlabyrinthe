unit CompilerMessages;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, SepiCompilerErrors;

type
  {*
    �v�nement portant sur une erreur de compilation Sepi
    @param Sender   Objet qui a d�clench� l'�v�nement
    @param Error    Erreur
  *}
  TSepiErrorEvent = procedure(Sender: TObject;
    Error: TSepiCompilerError) of object;

  {*
    Fen�tre des messages du compilateur
    @author sjrd
    @version 1.0
  *}
  TFormCompilerMessages = class(TForm)
    ListBoxMessages: TListBox;
    procedure ListBoxMessagesDblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FErrors: TSepiCompilerErrorList; /// Liste d'erreurs

    /// �v�nement d�clench� lorsqu'une erreur doit �tre montr�e
    FOnShowError: TSepiErrorEvent;

    procedure ErrorAdded(Sender: TObject; Error: TSepiCompilerError);
  public
    procedure Clear;

    property Errors: TSepiCompilerErrorList read FErrors;

    property OnShowError: TSepiErrorEvent read FOnShowError write FOnShowError;
  end;

implementation

{$R *.dfm}

{*
  Cr�ation de la fiche
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormCompilerMessages.FormCreate(Sender: TObject);
begin
  FErrors := TSepiCompilerErrorList.Create;
  FErrors.OnAddError := ErrorAdded;
end;

{*
  Destruction de la fiche
  @param Sender   Objet qui a d�clench� l'�v�nement
*}
procedure TFormCompilerMessages.FormDestroy(Sender: TObject);
begin
  FErrors.Free;
end;

{*
  Gestionnaire d'�v�nement OnAddError de la liste d'erreurs
  @param Sender   Objet qui a d�clench� l'�v�nement
  @param Error    Erreur ajout�e
*}
procedure TFormCompilerMessages.ErrorAdded(Sender: TObject;
  Error: TSepiCompilerError);
begin
  ListBoxMessages.Items.AddObject(Error.AsText, Error);
end;

{*
  Efface tous les messages
*}
procedure TFormCompilerMessages.Clear;
begin
  ListBoxMessages.Clear;
  Errors.Clear;
end;

{*
  Gestionnaire d'�v�nement OnDblClick de la liste des messages
*}
procedure TFormCompilerMessages.ListBoxMessagesDblClick(Sender: TObject);
var
  Index: Integer;
  Error: TSepiCompilerError;
begin
  Index := ListBoxMessages.ItemIndex;

  if (Index >= 0) and
    (ListBoxMessages.Items.Objects[Index] is TSepiCompilerError) then
  begin
    if Assigned(FOnShowError) then
    begin
      Error := TSepiCompilerError(ListBoxMessages.Items.Objects[Index]);
      FOnShowError(Self, Error);
    end;
  end;
end;

end.

