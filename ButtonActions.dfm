ÿ
 TFORMMODIFIEBOUTONS 0ð  TPF0TFormModifieBoutonsFormModifieBoutonsLeft_TopHelpContextBorderIconsbiSystemMenu BorderStylebsDialogCaptionActions des boutonsClientHeightÙClientWidth¡Color	clBtnFaceFont.CharsetDEFAULT_CHARSET
Font.ColorclWindowTextFont.Heightõ	Font.NameMS Sans Serif
Font.Style OldCreateOrder	PositionpoScreenCenterOnCreate
FormCreate	OnDestroyFormDestroyPixelsPerInch`
TextHeight TImageCarteLeft Top WidthÒ HeightÒ Stretch	OnMouseDownCarteMouseDown  TLabelLabelNoLeft°TopWidthgHeightCaption   Bouton(s) nÂ°Font.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Heightì	Font.NameMS Sans Serif
Font.Style 
ParentFont  TLabel
LabelEtageLeft4Topõ Width"HeightCaptionEtage :  TLabelLabelMessageLeftTopWidth÷ HeightCaption7   Texte de l'information que fait apparaÃ®tre ce bouton :  TLabelLabelRemplacementLeftð TopWidth}HeightCaptionCase(s) de remplacement :  TLabel
LabelStyleLeft°Top8WidthPHeightCaptionStyle du bouton :  TBevelSep1Left Top Width	HeightÙShape
bsLeftLine  TBevelSep3Left TopWidth¡Height	Shape	bsTopLine  TBevelSep4Left¢Top Width Height	Shape	bsTopLine  TBevelSep5Left TopIWidthHeightShape	bsTopLine  TLabelLabelSonLeft°Top WidthMHeightCaption   Ãmettre un son :  TBevelSep6Left¡TopØ Width Height	Shape	bsTopLine  TLabel	LabelBordLeft°Topà Width¹ HeightCaption*   Remplacer l'extÃ©rieur du labyrinthe par :  TLabelLabelNomBoutonLeft°TopPWidthOHeightCaptionNom du bouton :  TLabelLabelCouleurLeft°TopWidth HeightCaptionChanger la couleur du pion en :  
TScrollBar
HorizontalLeft TopÓ WidthÒ HeightLargeChangePageSize TabOrder OnScrollHorizontalScroll  
TScrollBarVerticalLeftÓ Top WidthHeightÒ Kind
sbVerticalLargeChangePageSize TabOrderOnScrollVerticalScroll  TSpinButtonBoutonsEtageLeftÓ TopÓ WidthHeightDownGlyph.Data
â   Þ   BMÞ       6   (   	            ¨                                                                                                               TabOrderUpGlyph.Data
â   Þ   BMÞ       6   (   	            ¨                            e                       d                                  V                      OnDownClickBoutonsEtageDownClick	OnUpClickBoutonsEtageUpClick  	TSpinEditEditNoLeft$TopWidthqHeight"EditorEnabledFont.CharsetDEFAULT_CHARSET
Font.ColorclBlackFont.Heightì	Font.NameMS Sans Serif
Font.Style MaxValue-MinValue 
ParentFontTabOrderValueOnChangeEditNoChange  	TSpinEdit	EditEtageLeft\Topð WidthIHeightMaxValueMinValueTabOrderValueOnChangeEditEtageChange  	TListView
CasesRempsLeftì TopWidth­ HeightÍ Columns IconOptions.ArrangementiaLeftIconOptions.WrapTextReadOnly	TabOrder	ViewStylevsSmallIcon  TButtonBoutonAjouterLeftí Topð WidthQHeightCaption
Ajouter...TabOrderOnClickBoutonAjouterClick  TButtonBoutonSupprimerLeftITopð WidthQHeightCaption	SupprimerTabOrderOnClickBoutonSupprimerClick  TMemoEditMessageLeftTop0WidthyHeightALines.StringsInformation 
ScrollBars
ssVerticalTabOrderWordWrapOnExitSaveMessage  	TCheckBoxCheckDesactiverLeft°TophWidthé HeightCaption,   DÃ©sactiver le bouton lorsqu'on passe dessusTabOrder	OnClickCheckDesactiverClick  TRadioGroupGroupMessageLeftTopxWidthyHeightACaptionType de l'information	ItemIndex Items.Strings   Message (toujours affichÃ©)J   Indice (affichÃ© seulement si l'option "Montrer les indices" est activÃ©e)K   Impasse (pour les obstacles ; n'est pas affichÃ© si on vient d'une flÃ¨che) TabOrder
OnClickGroupMessageClick  	TCheckBoxCheckUniqueLeftTopÄWidthqHeightCaptionL   L'information doit Ãªtre montrÃ©e seulement au premier passage sur le boutonTabOrderOnClickSaveMessage  	TComboBox
ComboStyleLeft$Top0WidthqHeightStylecsDropDownList
ItemHeightTabOrderOnChangeComboStyleChangeItems.StringsPoussoirCommutateur
Borne Info   Bouton cachÃ©Actions persoObjet persoObstacle persoDirection perso   TBitBtnBoutonOKLeftàTop Width Height)TabOrderKindbkOK  TBitBtnBoutonAnnulerLeftàTophWidth Height)TabOrderKindbkCancel  	TCheckBoxCheckUniqueSonLeft°TopÀ Widthé HeightCaption+   Ãmettre le son seulement la premiÃ¨re foisTabOrderOnClickComboSonChange  	TComboBox	ComboBordLeft°Topð Width± HeightStylecsDropDownList
ItemHeightTabOrderOnChangeComboBordChangeItems.Strings(Ne pas changer)SortieEauMursTrousHerbeCiel   	TComboBoxEditNomBoutonLeft$TopHWidthqHeight
ItemHeightSorted	TabOrderTextEditNomBouton  	TComboBoxComboSonLeft°Top  Widthé Height
ItemHeightTabOrderTextComboSon  	TComboBoxComboCouleurLeft°Top(Width± Height
ItemHeightTabOrderOnChangeComboCouleurChangeItems.Strings(Ne pas changer)BleuRougeVertJauneBlancNoir	Invisible    