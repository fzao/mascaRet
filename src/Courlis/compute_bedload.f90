Subroutine compute_bedload       (  &

        CVase           ,  & ! Concentration des vases en suspension
        CSable          ,  & ! Concentration des sables en suspension
        QVaseCouche     ,  & ! Flux de depot des vases par couche  (> 0 depot, < 0 erosion)
        QSableCouche    ,  & ! Flux de depot des sables par couche (> 0 depot, < 0 erosion)
        QVase           ,  & ! Flux de depot des vases  (> 0 depot, < 0 erosion)
        QSable          ,  & ! Flux de depot des sables (> 0 depot, < 0 erosion)
        QApportVase     ,  & ! Flux de d'apport lineaires des vases (Qapp*Capp) en (kg/s/m)
        QApportSable    ,  & ! Flux de d'apport lineaires des sables (Qapp*Capp) en (kg/s/m)
        TauH            ,  & ! Contrainte hydraulique locale (depend du tirant d'eau local)
        TauHMoy         ,  & ! Contrainte hydraulique moyenne dans la section
        TauHMax         ,  & ! Contrainte hydraulique maximale dans la section
        TauE            ,  & ! Contrainte hydraulique effective (depend du rayon hydr.)
        TauEMoy         ,  & ! Contrainte hydraulique effective moyenne ds section
        TauEMax         ,  & ! Contrainte hydraulique effective maximale ds section
        Ceq             ,  & ! Concentration d'equilibre des sables locale
        CeqMoy          ,  & ! Concentration d'equilibre des sables moyenne dans la section
!        DeltaH          ,  & ! Variation de hauteur sedimentaire en chaque point des profils
        ProfilCourlis   ,  & ! Profils sedimentaires
        CL_Vase         ,  & ! CL amont de la concentration en Vase
        CL_Sable        ,  & ! CL amont de la concentration en Sable
        ApportVase      ,  & ! Apports en vase
        ApportSable     ,  & ! Apports en sable
        Apport          ,  & ! Apports hydrauliques
        LoiHydrau       ,  & ! Lois hydrauliques
        LoiConc         ,  & ! Lois de concentration
        TempsCourlis    ,  & ! Temps du calcul
        TempsInitial    ,  & ! Premier temps
        DtCourlis       ,  & ! Pas de temps
        Zsurf1          ,  & ! Cote de la surface libre a t+dt
        Sm0             ,  & ! Surface mouillee a t
        Sm1             ,  & ! Surface mouillee a t+dt
        Vit1            ,  & ! Vitesse moyenne par section a t+dt
        Pm1             ,  & ! Perimetre mouille a t+dt
        CnuxV           ,  & ! Coefficient de diffusion vases
        CnuxS           ,  & ! Coefficient de diffusion Sables
        ConsConv        ,  & ! Parametres pour les schema de convection
        CoucheSed       ,  & ! Parametres sedimentaires des differentes couches
        LimiteDepotG    ,  & ! numero du point au dela duquel il peut y a voir depot ou erosion (1er pt immerge)
        LimiteDepotD    ,  & ! numero du dernier pt pour lequel il peut y a voir depot ou erosion (dernier pt immerge)
        LimiteSable     ,  & ! % de sable a partir duquel la couche est traitee suivant les lois du sable
        CalcSable       ,  & ! choix du calcul avec sable
        Erreur          )


!*************************************************************************
!  PROGICIEL : COURLIS           Ch. BERTIER - M. Jodeau
!
!  VERSION : 5.1       08-2009		Copyright EDF-CETMEF
!
!*************************************************************************
!=========================================================================
!
!  Fonction :	Calcule les termes sources  et resout l'equation de
!  --------		transport (convection - diffusion) des sediments dans l'eau
!
!  Sous-programme appelant : Courlis
!  -----------------------
!
!  Sous-programme appele :	CalcApport
!  ---------------------	Convec
!                               Sedimento
!                               Diffu
!
!=========================================================================

use M_PRECISION                 ! Definition de la precision DOUBLE ou SIMPLE
use M_PARAMETRE_C               ! Definition des constante tq EPS*, W0, ...

USE M_SHARE_VAR, ONLY: profil, profilplan

!use M_MY_GLOBAL_VAR             ! PU2017: Acces aux variables globales ProfilPlan et Profil
use M_MY_GLOBAL_VAR_SED         ! PU2017: Acces aux variables globales Vsed et Hsed
use M_CSUR_I                    ! PU2017: Ajout pour mise a jour du planimetrage
use M_CSURM1_I                  ! PU2017: Ajout pour mise a jour du planimetrage

use M_PROFIL_COURLIS_T          ! Definition du type PROFIL_COURLIS
use M_COUCHE_T                  ! Definition du type COUCHE_T
use M_APPORT_T                  ! Definition du type APPORT_T
use M_SOURCE_TRACER_T           ! Donnees des sources d'un traceur
use M_CL_COURLIS_T              ! Definition du type CL_COURLIS_T
use M_LOI_T                     ! Definition du type LOI_T
use M_LOI_CONC_T                ! Definition du type LOI_T
use M_CONSTANTES_TRACER_T   ! Donnees pour le schema de convection

use M_ERREUR_T                  ! Type ERREUR_T
use M_MESSAGE_C                 ! Messages d'erreur
use M_TRAITER_ERREUR_I          ! Traitement de l'errreur

use M_CalcApport_I
use M_Convec_I
use M_Sedimento_I
use M_Diffu_I

use M_INTERPOLATION_S           ! Sous-programme INTERPOLATION_S


!=========================================================================
! DECLARATIONS
!=========================================================================

!.. Implicit Declarations ..
  implicit none

! Variables d'entree
  real(DOUBLE)             :: DtCourlis, TempsCourlis, TempsInitial
  real(DOUBLE), intent(in) :: CnuxV
  real(DOUBLE), intent(in) :: CnuxS
  logical                  :: CalcSable

  real(DOUBLE)                  , dimension(:)  , intent(in   ) :: Zsurf1, Sm0, Sm1, Pm1
  real(DOUBLE)                                  , intent(in   ) :: LimiteSable
  integer                       , dimension(:)  , intent(in   ) :: LimiteDepotG, LimiteDepotD
  type(COUCHE_T)                , dimension(:)  , intent(in   ) :: CoucheSed
  type(LOI_T)                   , dimension(:)  , intent(in   ) :: LoiHydrau
  type(LOI_CONC_T)              , dimension(:)  , intent(in   ) :: LoiConc

! Variables de sortie
  type(PROFIL_COURLIS_T), dimension(:)  , intent(inout) :: ProfilCourlis
  type(CL_COURLIS_T)                    , intent(inout) :: CL_Vase, CL_Sable
  type(SOURCE_TRACER_T),  dimension(:)  , intent(inout) :: ApportVase, ApportSable
  type(APPORT_T)                , dimension(:)  , intent(inout) :: Apport
  real(DOUBLE)                  , dimension(:)  , intent(inout) :: CVase, CSable
  real(DOUBLE)                  , dimension(:,:), intent(inout) :: QVaseCouche, QSableCouche
  real(DOUBLE)                  , dimension(:)  , intent(  out) :: QVase, QSable
  real(DOUBLE)                  , dimension(:)  , intent(  out) :: QApportVase, QApportSable
  real(DOUBLE)                  , dimension(:,:), intent(  out) :: TauH, TauE, Ceq
  real(DOUBLE)                  , dimension(:)  , intent(  out) :: TauHMoy, TauHMax, TauEMoy, TauEMax, CeqMoy
!  real(DOUBLE)                  , dimension(:,:), intent(inout) :: DeltaH
  type(CONSTANTES_TRACER_T)                 , intent(inout) :: ConsConv
  real(DOUBLE)                  , dimension(:)  , intent(inout) :: Vit1

! Variables locales
  integer :: i, k         ! Compteurs  !, iapp
  integer :: NbProfil           ! Nombre de profils
  integer :: NbCouche           ! Nombre de couches sedimentaires
  integer :: NbApport           ! Nombre d'apports

! AJOUT PU2016
  integer      :: j
  integer      :: NPt                                   ! Nombre de point de la section en travers i
  integer      :: NPtmax                                ! Nombre de point max
  real(DOUBLE) :: NPtd                                  ! Nombre de points de la section en travers i (double)
  real(DOUBLE) :: Rho, RhoS                   ! constantes du calcul  !, W52, W14
  real(DOUBLE) :: V, Sm, Rh, Rh13  ! variables hydrauliques du profil !H13,Rh43,H,Zsl
  real(DOUBLE) :: V2
  real(DOUBLE) :: Kp, Kt, d50      ! parametres sedimentaires de la couche affleurante ! Cf,M,Ps,Tce,W
  real(DOUBLE) :: d50m                                  ! diametre de grain moyen
  real(DOUBLE) :: TaH, TauHMa, TauHMo, TaE, TauEMo, TauEMa !, Ce, CeqMo ! variables intermÃ©diaires de calcul
  real(DOUBLE) :: HDepo                   ! variables intermÃ©diaires de calcul (flux, volume, hauteur de dÃ©pÃ´t) !Depo,VolDep
  real(DOUBLE), dimension(:)  , allocatable :: dxj       ! pas d'abscisse angulaire du profil
  integer     , dimension(:)  , allocatable :: Couche   ! numero de la 1ere couche non vide en chaque point de profil
  real(DOUBLE), dimension(:)  , allocatable :: Yi,Yj    ! abscisse longitudinale
  real(DOUBLE) :: minZb1, minZb2                        ! Cote du point bas
  real(DOUBLE), dimension(:)  , allocatable :: Fr       ! Nombre de Froude
  real(DOUBLE), dimension(:)  , allocatable :: Az       ! Volume de sediment depose par profil
  real(DOUBLE), dimension(:)  , allocatable :: dAz      ! Variation de volume de sediment depose par profil
  real(DOUBLE), dimension(:)  , allocatable :: Qsed     ! Capacite de transport par section
  real(DOUBLE) :: R, RG, Zsl, Z2                        ! Variables temporaires
  real(DOUBLE), dimension(:)  , allocatable :: Hroe     ! Hauteur d'eau (Roe)
  real(DOUBLE), dimension(:)  , allocatable :: Uroe     ! Vitesse (Roe)
  real(DOUBLE) :: H1,H2                                 ! Hauteurs d'eau temporaires
  real(DOUBLE) :: Tiro                            ! Variables planimetrees
  real(DOUBLE), dimension(:)  , allocatable :: DZP      ! Tableau pas de planimetrage
  real(DOUBLE) :: B1
  integer :: nb_pas

  integer , parameter :: ORDRE_INTERPOLATION = 1
  integer :: num_loi            ! Numero de la loi utilisee


! Traitement des erreurs
  type(ERREUR_T), intent(inout) :: Erreur
  type(ERREUR_T) :: Erreur1
  integer                       :: retour  ! Code de retour de la fonction read, allocate

! MS2018 gestion des fonds durs (erosion)
!variables a declarer NodeWidth, HEros, HErosPot, EroPot, SedWidth, LayerThick, VolAvail
  real(DOUBLE)                              :: NodeWidth, HEros, HErosPot, EroPot
  real(DOUBLE)                              :: SedWidth, LayerThick, VolAvail

!=========================================================================

  Erreur%Numero = 0
  Erreur%arbredappel = trim(Erreur%arbredappel)//'=>DansLo'

! Initialisations des constantes
! ------------------------------
  Rho  = 1000._DOUBLE
  RhoS = 2650._DOUBLE
  R    = (RhoS-Rho)/Rho
  RG   = R*Gpes


!=========================================================================
! Initialisation et allocation des tableaux locaux
!=========================================================================

  IF(TempsCourlis == TempsInitial) THEN
    WRITE(*,*)
    WRITE(*,*)
    WRITE(*,*) "Loi de transport utilisee : MPM Fonds durs"
    WRITE(*,*)
  ENDIF

  NbProfil = size(ProfilCourlis)
  NbCouche = size(CoucheSed)
  NbApport = size(Apport)

  NPtmax = 0
  Do i = 1, NbProfil
    NPtmax = max(NPtmax, ProfilCourlis(i)%NbPoint)
  Enddo

  Allocate(DZP(NbProfil),STAT=retour)

  Allocate(dxj(NbProfil),STAT=retour)
  Allocate(Hroe(NbProfil+1),STAT=retour)
  Allocate(Uroe(NbProfil+1),STAT=retour)
  Allocate(Fr(NbProfil+1),STAT=retour)
  Allocate(Az(NbProfil+2),STAT=retour)
  Allocate(dAz(NbProfil),STAT=retour)
  Allocate(Qsed(NbProfil+2),STAT=retour)

  Allocate(Yi(NbProfil+2),STAT=retour)

  Allocate(Yj(NbProfil),STAT=retour)
  Yj = ProfilCourlis%Abs
  Do i = 1, NbProfil
    QVase(i)  = W0
    QSable(i) = W0
    Do k = 1, NbCouche
      QVaseCouche(k,i)  = W0
      QSableCouche(k,i) = W0
    Enddo
    If (NbApport /= 0) Then
      QApportVase(i)  = W0
      QApportSable(i) = W0
    Endif
  Enddo

!=========================================================================
! Calcul des termes sources par apport et conditions limites
!=========================================================================
  num_loi = CL_Sable%NumeroLoi


  call INTERPOLATION_S                  ( &
         CL_Sable%Conc                  , &
         TempsCourlis                   , &
         ORDRE_INTERPOLATION            , &
         LoiConc(num_loi)%Temps         , &
         LoiConc(num_loi)%Conc          , &
         size(LoiConc(num_loi)%Temps)   , &
         Erreur                         )


  If (Erreur%Numero /= 0) Then
    return
  End if

!=========================================================================
! Boucle sur les profils
!=========================================================================
  Do i = 1, NbProfil
    ! Initialisations
    TauHMa = W0
    TauHMo = W0
    TauEMa = W0
    TauEMo = W0
    d50m   = w0

    nb_pas = Profil(i)%NbPas
    DZP(i) = Profil(i)%Pas
    !Utilisation de CSUR pour interpoler la largeur au miroir au niveau d'eau actuel
    B1 = CSUR ( i, ZSurf1(i)-(ProfilCourlis(i)%Zref(1)), DZP, ProfilPlan%B1, nb_pas, Erreur1 )
    dxj(i) = 1.D0 / B1 ! inverse de la Largeur au miroir (pour simplifier les formules de transport ??)

    ! Allocation de memoire des tableaux locaux
    ! -----------------------------------------
    NPt = ProfilCourlis(i)%NbPoint

    allocate(Couche(NPt),STAT=retour)
    If (retour /= 0) Then
      Erreur%Numero = 5
      Erreur%ft   = err_5
      Erreur%ft_c = err_5c
      call TRAITER_ERREUR (Erreur, 'Couche')
      return
    End if

    ! Conditions hydrauliques dans la section
    V  = Vit1(i)
    Sm = Sm1(i)
    If (Sm < W0) Then
      Erreur%Numero = 415
      Erreur%ft   = err_415
      Erreur%ft_c = err_415c
      call TRAITER_ERREUR (Erreur, i)
      return
    Endif
    Rh = max(Sm/Pm1(i), EPS8)

    Rh13 = Rh**(1._DOUBLE/3._DOUBLE)
    V2 = V*V


    Yi(i+1) = Yj(i)

    ! Initialisation des variables aux points de la section en travers
    Do j = 1,NPt
      TauH(j,i)  = W0
      TauE(j,i)  = W0
      Ceq(j,i)   = W0
      Couche(j)  =  0
      Do k = 1, NbCouche
      !on arrete a la premiere couche non nulle (goto 10)
        If ((ProfilCourlis(i)%Z(k,j)-ProfilCourlis(i)%Z(k+1,j)) > EPS5) Then
          Couche(j) = k
          goto 10
        Endif
      Enddo
10 continue
    Enddo

    NPtd = 0.D0

    ! Calcul des contraintes de frottement
    ! ------------------------------------
    Do j = 1, ProfilCourlis(i)%NbPoint

      If (ProfilCourlis(i)%Z(2,j)<ZSurf1(i)) Then

      NPtd = NPtd + 1.D0

      k = Couche(j)
      If (k /= 0) Then
        Kp  = CoucheSed(k)%Kp
        Kt  = CoucheSed(k)%Kt
        d50 = CoucheSed(k)%D50
      Else
        Kp  = CoucheSed(1)%Kp
        Kt  = CoucheSed(2)%Kt
        d50 = CoucheSed(2)%D50
      Endif

      ! Contrainte de frottement pour les vases
      TaH = Rho * Gpes  * V2 / (Kp*Kp * Rh13)
      d50m = d50m + d50
      ! DEBUT MPM -------------------------------------------------------------------------------
      ! Contrainte  ADIM pour les GRAVIERS et capacité de transport (MPM)
      TaE = V2 / (R * Kt*Kt * Rh13 * d50 ) * (Kt/Kp)**(W32)
      ! FIN MPM -------------------------------------------------------------------------------

      TauH(j,i) = TaH
      If (TaH > TauHMa)   TauHMa = TaH
      TauHMo    = TauHMo + TaH

      TauE(j,i) = TaE
      If (TaE > TauEMa)   TauEMa = TaE
      TauEMo    = TauEMo + TaE

      Endif

    Enddo
    NPtd = 1.D0/NPtd

    TauHMax(i) = TauHMa
    TauHMoy(i) = TauHMo * NPtd
    TauEMax(i) = TauEMa
    TauEMoy(i) = TauEMo * NPtd
    !Attention NPtd = 1/NPtd // gestion d'un d50 différent entre
    d50m = d50m * NPtd

    ! MPM
    if (TauEMoy(i) > 0.047_DOUBLE ) then
      Qsed(i+1) =  (8_DOUBLE*sqrt( RG*d50m**3_DOUBLE ) &
                        *(TauEMoy(i)-0.047_DOUBLE)**(3._DOUBLE/2._DOUBLE)) / dxj(i)
    else
      Qsed(i+1) =  w0
    endif

    Deallocate ( Couche )
  Enddo


  Yi(1)          = Yi(2) - (Yi(3)-Yi(2))
  Yi(NbProfil+2) = Yi(NbProfil+1) + (Yi(NbProfil+1)-Yi(NbProfil))


  ! Conditions limites
  !-------------------
  Qsed(1)          = (CL_Sable%Conc)*Vit1(1)*Sm1(1) / RhoS  ! Cs*Q1/RhoS

  Qsed(NbProfil+2) = Qsed(NbProfil+1)


  ! Construction des etats de Roe et Calcul du nombre de Froude aux interfaces
  !---------------------------------------------------------------------------
  Do i=2,NbProfil
    H1 = Sm1(i-1) * dxj(i-1)
    H2 = Sm1(i  ) * dxj(i)
    Hroe(i) = sqrt( H1*H2 )
    Uroe(i) = ( sqrt(H1)*Vit1(i-1) + sqrt(H2)*Vit1(i) )/( sqrt(H1) + sqrt(H2) )
    Fr(i) = Uroe(i) / sqrt(Gpes*Hroe(i))
  Enddo

  Hroe(1) = Sm1(1) * dxj(1)
  Uroe(1) = Vit1(1)
  Fr(1)   = Uroe(1) / sqrt(Gpes*Hroe(1))

  Hroe(NbProfil+1) = Sm1(NbProfil) * dxj(NbProfil)
  Uroe(NbProfil+1) = Vit1(NbProfil)
  Fr(NbProfil+1)   = Uroe(NbProfil+1) / sqrt(Gpes*Hroe(NbProfil+1))

  ! Maj des cotes de fond: Application du schema VF
  !------------------------------------------------
  Do i = 1,NbProfil

    if ( (Fr(i)>1._DOUBLE).AND.(Fr(i+1)>1._DOUBLE) ) then
      dAz(i) = - DtCourlis*(Qsed(i+2)-Qsed(i+1))/(Yi(i+1)-Yi(i))
    else if ( (Fr(i)<1._DOUBLE).AND.(Fr(i+1)<1._DOUBLE) ) then
      dAz(i) = - DtCourlis*(Qsed(i+1)-Qsed(i))/(Yi(i+1)-Yi(i))
    else if ( (Fr(i)<1._DOUBLE).AND.(Fr(i+1)>1._DOUBLE) ) then
      dAz(i) = - DtCourlis*0.5D0*(Qsed(i+2)-Qsed(i))/((Yi(i+1)-Yi(i)))
    else
      dAz(i) = w0
    endif

    minZb1 = INFINI
    minZb2 = INFINI

    if (abs(dAz(i)) < 1.E-10) then
      HDepo = w0
    else
    !Gestion de la porosite
      HDepo = dAz(i) * Rhos / CoucheSed(1)%Cfond ! * dxj(i) ! Volume de sediments transportes
    endif

    Vsed(i) = HDepo

    NPt = ProfilCourlis(i)%NbPoint
    Zsl = Zsurf1(i)

    If (HDepo > 0.D0) then  ! Cas Depot
      Tiro = CSURM1( HDepo, Profil(i)%Pas, ProfilPlan%S1(i,:), Erreur1 )

      If (Tiro < 0.D0)  Then
        Tiro    = 0.D0
        Vsed(i) = 0.D0
      Endif


      HDepo = HDepo * dxj(i)

      Do j = 1, NPt
        Z2 = ProfilCourlis(i)%ZRef(1)+Tiro


        if (ProfilCourlis(i)%Z(1,j)<Z2) then
          ProfilCourlis(i)%Z(1,j) = Z2
          ProfilCourlis(i)%Z(2,j) = Z2
        endif

        minZb1 = min(ProfilCourlis(i)%Z(1,j),minZb1)
        minZb2 = min(ProfilCourlis(i)%Z(2,j),minZb2)

      Enddo

      DeltaH(1,i) = HDepo
      Hsed(i) = Tiro

    Else If (HDepo < 0.D0) then ! Cas Erosion
      SedWidth = 0.D0
      Do j = LimiteDepotG(i), LimiteDepotD(i)
        LayerThick = Abs(ProfilCourlis(i)%Z(1,j) - ProfilCourlis(i)%Z(NbCouche+1,j))

        If (j == 1) Then
          NodeWidth  = Abs((ProfilCourlis(i)%X(2) - ProfilCourlis(i)%X(1)) / W2)
        Else If (j == NPt) Then
          NodeWidth  = Abs((ProfilCourlis(i)%X(NPt) - ProfilCourlis(i)%X(NPt-1)) / W2)
        Else
          NodeWidth  = Abs((ProfilCourlis(i)%X(j+1) - ProfilCourlis(i)%X(j-1)) / W2)
        Endif
        If(LayerThick > EPS5)Then
          SedWidth = SedWidth + NodeWidth
        Endif
      Enddo
      !MS2019 ajout en cas d'absence de sediment
      IF (SedWidth .GT. 0) THEN
        HErosPot = - HDepo / SedWidth
      ELSE
        HErosPot = 0
      ENDIF


      Do j = LimiteDepotG(i), LimiteDepotD(i)

        If (j == 1) Then
          NodeWidth  = (ProfilCourlis(i)%X(2) - ProfilCourlis(i)%X(1)) / W2
        Else If (j == NPt) Then
          NodeWidth  = (ProfilCourlis(i)%X(NPt) - ProfilCourlis(i)%X(NPt-1)) / W2
        Else
          NodeWidth  = (ProfilCourlis(i)%X(j+1) - ProfilCourlis(i)%X(j-1)) / W2
        Endif

        EroPot = HErosPot * NodeWidth
        LayerThick = Abs(ProfilCourlis(i)%Z(1,j) - ProfilCourlis(i)%Z(NbCouche+1,j))
        VolAvail = LayerThick * NodeWidth

        If (EroPot > VolAvail)  EroPot = VolAvail ! Erosion limited by layer thickness

        HEros = EroPot / NodeWidth

        if (ProfilCourlis(i)%Z(1,j)<Zsl) then
          ProfilCourlis(i)%Z(1,j) = ProfilCourlis(i)%Z(1,j) - HEros
          ProfilCourlis(i)%Z(2,j) = ProfilCourlis(i)%Z(2,j) - HEros
        endif

        minZb1 = min(ProfilCourlis(i)%Z(1,j),minZb1)
        minZb2 = min(ProfilCourlis(i)%Z(2,j),minZb2)


      Enddo

      HDepo = HDepo * dxj(i)
      DeltaH(1,i) = HDepo
      Hsed(i) = HDepo

    Else

      minZb1 = ProfilCourlis(i)%Zref(1)
      minZb2 = ProfilCourlis(i)%Zref(2)
      Hsed(i) = 0.0D0

    Endif

    ProfilCourlis(i)%Zref(1) = minZb1
    ProfilCourlis(i)%Zref(2) = minZb2

  Enddo

! Calcul des flux lineaires
! -------------------------
  Do i = 1, NbProfil
    Do k = 1, NbCouche
      QSable(i) = Qsed(i+1)*1.E6
    Enddo
    If (NbApport /= 0) Then
      QSable(i) = Qsed(i+1)*1.E6
    Endif
  Enddo

  NIteSed = NIteSed + 1  ! PU2017: Incrementation du compteur de calcul de mise a jour du fond

!=========================================================================
!	Deallocation des tableaux locaux
!=========================================================================
  Deallocate (Yi, Yj, Az, Qsed, dAz, Fr, Hroe, Uroe, dxj)
  Deallocate (DZP)

  return

End Subroutine compute_bedload
