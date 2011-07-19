{*
  Constantes globales de FunLabyrinthe
  @author sjrd
  @version 5.0
*}
unit FunLabyCoreConsts;

interface

const {don't localize}
  CurrentVersion = '5.2~beta2'; /// Version courante de FunLabyrinthe
  CurrentMinorVersion = 1;      /// Version mineure courante de FunLabyrinthe
  FunLabyAuthorName = 'S�bastien Jean Robert Doeraene'; /// Auteur
  FunLabyAuthorEMail = 'sjrd@redaction-developpez.com'; /// E-mail de l'auteur
  FunLabyWebSite = 'http://www.funlabyrinthe.com/';     /// Site Web

resourcestring
  // Noms de composants
  SDefaultObjectInfos = '%s : %d';
  SNothing = 'Rien';
  SEffectName = '%1:s sur %0:s';
  SToolName = '%s avec %s';
  SObstacleName = '%s obstru� par %s';
  SWhichObject = 'Quel objet voulez-vous utiliser ?';
  SDefaultPlayerName = 'Joueur';
  SComponentCreatorHint = 'Cr�er un nouveau composant de type %s';

  // Noms des cat�gories standard
  SCategoryFields = 'Terrains';
  SCategoryEffects = 'Effets';
  SCategoryTools = 'Outils';
  SCategoryObstacles = 'Obstacles';
  SCategoryNeutrals = 'Neutres';
  SCategoryVehicles = 'V�hicules';
  SCategoryPlayers = 'Joueurs';
  SCategoryObjects = 'Objets';
  SCategoryPlugins = 'Plug-in';
  SCategoryComponentCreators = 'Composants suppl�mentaires';

  // Erreurs de composants
  SComponentNotFoundTitle = 'Composant non trouv�';
  SComponentNotFound = 'Le composant d''ID %s n''existe pas';
  SInvalidComponentTitle = 'Composant invalide';
  SInvalidComponent = 'Le composant d''ID %s n''est pas valide';
  SRepareComponentError =
    'Voulez-vous ignorer cette erreur et tenter de continuer ?';
  SEditingRequiredForSetID =
    'Impossible de modifier l''ID d''un composant sauf en �dition';
  SAdditionnalRequiredForSetID =
    'Impossible de modifier l''ID d''un composant cr�� automatiquement';
  SInvalidID = '''%s'' n''est pas un ID de composant valide (seuls les '+
    'lettres non accentu�es, les chiffres et le _ sont autoris�s)';
  SDuplicateID = 'Il existe d�j� un composant dont l''ID est %s';
  SUnsupportedCommand = 'La commande %s n''est pas support�e';
  STemporaryStatedMap =
    'La carte d''ID %s est dans un �tat temporaire qui ne peut �tre enregistr�';

  // Titres des messages
  SDescription = 'Description';
  SMessage = 'Message';
  STip = 'Indice';
  SChoice = 'Choix';
  SError = 'Erreur';
  SFailure = '�chec';
  SBlindAlley = 'Impasse';
  SWon = 'Gagn� !';
  SLost = 'Perdu !';

  // Formats de fichier et traitement des fichiers
  SFullVersionNumber = '%s (build %d)';
  SInvalidFileFormat = 'Le fichier n''est pas un document FunLabyrinthe valide';
  SVersionTooHigh = 'Le fichier a �t� enregistr� avec une version ult�rieure '+
    'de FunLabyrinthe (v%s). Il ne peut �tre ouvert.';
  SFileNotFoundTitle = 'Fichier introuvable';
  SFileNotFound = 'Le fichier sp�cifi� "%s" n''existe pas';
  SCannotMakeHRef = 'Impossible de construire un href pour le fichier ''%s'' '+
    'car celui-ci n''est pas dans le chemin de recherche du projet';
  SResourceNotFound = 'La ressource sp�cifi�e "%s" n''existe pas';
  SCannotMakeResourceHRef = 'Impossible de construire un href pour la '+
    'ressource ''%s'' car celle-ci n''est pas dans le chemin de recherche du '+
    'projet';
  SUnknownUnitType = 'Type d''unit� ''%s'' inconnu';
  SThereMustBeOnePlayer = 'Il doit y avoir un et un seul joueur par fichier';
  SEditingNotAllowed = 'L''�dition de ce fichier n''est pas permise';
  SCantEditSaveguard = 'L''�dition d''une sauvegarde est impossible';
  SNoFileName = 'Aucun nom de fichier sp�cifi�';
  SCantLoadPackage = 'Impossible de charger le paquet "%s"';
  SPainterFilter = 'Peintre FunLabyrinthe|*.pnt';

implementation

end.

