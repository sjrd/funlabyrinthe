object FrameDelphiSourceEditor: TFrameDelphiSourceEditor
  Left = 0
  Top = 0
  Width = 499
  Height = 244
  TabOrder = 0
  TabStop = True
  object EditSource: TSynEdit
    Left = 0
    Top = 0
    Width = 499
    Height = 244
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    OnKeyDown = EditSourceKeyDown
    OnMouseDown = EditSourceMouseDown
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Highlighter = DelphiHighlither
    Lines.Strings = (
      'unit DelphiSourceEditorMain;'
      ''
      'interface'
      ''
      'uses'
      
        '  Windows, Messages, SysUtils, Variants, Classes, Graphics, Cont' +
        'rols, Forms,'
      '  Dialogs, FilesUtils, UnitFiles, UnitEditorIntf, StdCtrls;'
      ''
      'resourcestring'
      '  DelphiFilter = '#39'Unit'#233' Delphi (*.pas)|*.pas'#39';'
      ''
      'const {don'#39't localize}'
      '  DelphiExtension = '#39'pas'#39';'
      ''
      'type'
      '  {*'
      '    '#201'diteur de source Delphi'
      '    @author sjrd'
      '    @version 5.0'
      '  *}'
      '  TFrameDelphiSourceEditor = class(TFrame, ISourceEditor50)'
      '    EditSource: TSynEdit;'
      '    DelphiHighlither: TSynPasSyn;'
      '  private'
      '    FSourceFile: TSourceFile; /// Fichier source'
      ''
      '    procedure LoadFile(ASourceFile: TSourceFile);'
      ''
      '    function GetSourceFile: TSourceFile;'
      '    function GetControl: TControl;'
      '    function CanClose: Boolean;'
      '    procedure ISourceEditor50.Release = Free;'
      '  public'
      '    property SourceFile: TSourceFile read FSourceFile;'
      '  end;'
      ''
      'implementation'
      ''
      '{$R *.dfm}'
      ''
      'const'
      '  CreateDelphiSourceInfo: TSourceFileCreatorInfo = ('
      '    Title: '#39'Source Delphi'#39';'
      
        '    Description: '#39'Fichier source Delphi, le langage original de ' +
        'FunLabyrinthe'#39';'
      '    AskForFileName: True;'
      '    Filter: DelphiFilter'
      '  );'
      ''
      '{*'
      '  Cr'#233'e un '#233'diteur de source Delphi'
      '  @param UnitFile   Fichier unit'#233' '#224' '#233'diter'
      '  @return Interface vers l'#39#233'diteur cr'#233#233
      '*}'
      
        'function CreateDelphiSourceEditor(SourceFile: TSourceFile): ISou' +
        'rceEditor50;'
      'var'
      '  Editor: TFrameDelphiSourceEditor;'
      'begin'
      '  Editor := TFrameDelphiSourceEditor.Create(nil);'
      '  Editor.LoadFile(TSourceFile(SourceFile));'
      '  Result := Editor as ISourceEditor50;'
      'end;'
      ''
      '{*'
      '  Cr'#233'e un nouveau fichier source Delphi'
      '  @param FileName   Nom du fichier source'
      '  @return True si le fichier a '#233't'#233' cr'#233#233', False sinon'
      '*}'
      'function CreateDelphiSource(var FileName: TFileName): Boolean;'
      'var'
      '  UnitName: string;'
      'begin'
      '  UnitName := ChangeFileExt(ExtractFileName(FileName), '#39#39');'
      ''
      '  with TStringList.Create do'
      '  try'
      '    Add(Format('#39'unit %s;'#39', [UnitName]));'
      '    Add('#39#39');'
      '    Add('#39'interface'#39');'
      '    Add('#39#39');'
      '    Add('#39'implementation'#39');'
      '    Add('#39#39');'
      '    Add('#39'end.'#39');'
      ''
      '    SaveToFile(FileName);'
      '  finally'
      '    Free;'
      '  end;'
      ''
      '  Result := True;'
      'end;'
      ''
      '{-------------------------------}'
      '{ Classe TFrameDelphiUnitEditor }'
      '{-------------------------------}'
      ''
      '{*'
      '  Charge un fichier source'
      '  @param ASourceFile   Fichier source '#224' charger'
      '*}'
      
        'procedure TFrameDelphiSourceEditor.LoadFile(ASourceFile: TSource' +
        'File);'
      'begin'
      '  FSourceFile := ASourceFile;'
      ''
      '  EditSource.Lines.LoadFromFile(SourceFile.FileName);'
      'end;'
      ''
      '{*'
      '  [@inheritDoc]'
      '*}'
      'function TFrameDelphiSourceEditor.GetSourceFile: TSourceFile;'
      'begin'
      '  Result := FSourceFile;'
      'end;'
      ''
      '{*'
      '  [@inheritDoc]'
      '*}'
      'function TFrameDelphiSourceEditor.GetControl: TControl;'
      'begin'
      '  Result := Self;'
      'end;'
      ''
      '{*'
      '  [@inheritDoc]'
      '*}'
      'function TFrameDelphiSourceEditor.CanClose: Boolean;'
      'begin'
      '  Result := True;'
      'end;'
      ''
      'initialization'
      '  SourceEditors.Add(DelphiExtension, CreateDelphiSourceEditor);'
      '  SourceEditors.AddFilter(DelphiFilter);'
      ''
      
        '  SourceFileCreators.Add(CreateDelphiSource, CreateDelphiSourceI' +
        'nfo);'
      'finalization'
      '  SourceEditors.Remove(DelphiExtension);'
      '  SourceEditors.RemoveFilter(DelphiFilter);'
      ''
      '  SourceFileCreators.Remove(CreateDelphiSource);'
      'end.')
    Options = [eoAltSetsColumnMode, eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo, eoScrollPastEol, eoShowScrollHint, eoSmartTabDelete, eoSmartTabs, eoTabsToSpaces, eoTrimTrailingSpaces]
    OnChange = EditSourceChange
    OnSpecialLineColors = EditSourceSpecialLineColors
  end
  object DelphiHighlither: TSynPasSyn
    CommentAttri.Foreground = clGreen
    DirectiveAttri.Foreground = clNavy
    DirectiveAttri.Style = [fsBold]
    KeyAttri.Foreground = clNavy
    NumberAttri.Foreground = clBlue
    FloatAttri.Foreground = clBlue
    HexAttri.Foreground = clBlue
    StringAttri.Foreground = clBlue
    CharAttri.Foreground = clBlue
    Left = 80
    Top = 56
  end
end
