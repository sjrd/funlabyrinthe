unit FunLabyEditConsts;

interface

resourcestring
  SErrorTitle = 'Erreur';
  SFatalErrorTitle = 'Erreur fatale';

  SBaseSepiRootLoadError =
    'Erreur au chargement des fonctionnalit�s coeur de FunLabyrinthe avec le '+
    'message "%s". FunLabyrinthe ne peut continuer et doit fermer.';

  SFeatureIsNotImplementedYet = 'Cette fonction n''est pas encore impl�ment�e';

  SDefaultPlayerName = 'Joueur';

  SPlayerPosition = 'Position : %s';
  SCenterToPosition = 'Centrer la vue sur ce joueur';
  SShowPlugins = 'Plug-in...';
  SShowAttributes = 'Attributs...';
  SShowObjects = 'Objets...';

  SConfirmExitTitle = 'Enregistrer le fichier';
  SConfirmExit = 'Le fichier a �t� modifi�. Voulez-vous l''enregistrer ?';

  SCantSave = 'Impossible d''enregistrer';
  SCantSaveUnplacedPlayer =
    'Impossible d''enregistrer car le joueur d''ID %s n''a pas �t� plac�';

  SCantCenterToPosition = 'Impossible de centrer le joueur';
  SCantCenterToUnplacedPlayer =
    'Impossible de centrer sur ce joueur car il n''a pas �t� plac�';

  SOnlyFieldOutsideTitle = 'Remplacer l''ext�rieur de la carte';
  SOnlyFieldOutside =
    'Seuls les terrains sont accept�s en dehors de la carte';

  SRemoveMapTitle = 'Supprimer une carte';
  SRemoveMap =
    '�tes-vous certain de vouloir supprimer compl�tement cette carte ?';

  SDuplicateSourceTitle = 'Fichier source d�j� pr�sent';
  SDuplicateSource = 'Ce fichier source est d�j� pr�sent dans le projet';

  SRemoveSourceTitle = 'Retirer un fichier source';
  SRemoveSource =
    '�tes-vous certain de vouloir retirer ce fichier source du projet ?';
  SErrorWhileOpeningSourceFile =
    'Une erreur est survenu lors de l''ouverture du fichier avec le message '+
    '"%s". Voulez-vous retirer ce fichier source du projet ?';

  SAllUnitTypes = 'Tous les types d''unit�';

  SDuplicateUnitTitle = 'Unit� d�j� pr�sente';
  SDuplicateUnit = 'Il y a d�j� une unit� ''%s''';

  SRemoveUnitTitle = 'Supprimer une unit�';
  SCantRemoveLastUnit = 'Il faut toujours garder au moins une unit�';
  SConfirmRemoveUnit = '�tes-vous s�r-e de vouloir supprimer l''unit� ''%s'' ?';

  SComponentDataTitle = 'Propri�t�s du composant';
  SPlayerDataTitle = 'Propri�t�s par joueur';
  SPlayerAttributesTitle = 'Attributs du joueur';
  SPlayerPluginsTitle = 'Plug-in attach�s au joueur';

  SCollectionItemTitle = '%d - %s';

  SCreateCompChooseIDTitle = 'Cr�ation d''un nouveau composant';
  SCreateCompChooseID = 'Veuillez choisir l''ID du nouveau composant';

  SInvalidComponentIDTitle = 'ID de composant invalide';
  SBadComponentID =
    'L''ID ne doit contenir que des lettres non accentu�es et des chiffres';
  SComponentAlreadyExists = 'Il existe d�j� un composant avec cet ID';

  SInvalidFileNameTitle = 'Nom de fichier invalide';
  SInvalidUnitName =
    '%s n''est pas un nom d''unit� valide, il ne doit contenir que des '+
    'lettres non accentu�es et des chiffres, et commencer par une lettre';
  SInvalidExtension =
    'L''extension %s n''est pas valide pour ce type de fichier';

  SNoFieldTitle = 'Aucun terrain';
  SNoField =
    'Impossible de lancer cette op�ration car ce labyrinthe ne contient '+
    'aucun terrain';

  SResizeErrorTitle = 'Erreur de redimensionnement';
  SCantResizeToEmpty = 'Impossible de r�tr�cir plus la carte';

  SConfirmDeleteComponentTitle = 'Supprimer le composant';
  SConfirmDeleteComponent =
    '�tes-vous s�r-e de vouloir supprimer ce composant ?';

implementation

end.

