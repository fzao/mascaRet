Module M_TALUS_T

use M_PRECISION

  TYPE TALUS_T
     sequence
     integer                     :: Modele     ! Choix du modele de traitement des berges
											   ! (1 => pente ; 2 => glissement)
     real(DOUBLE)                :: PstabI     ! pente de stabilité des talus immergés
     real(DOUBLE)                :: PstabE     ! pente de stabilité des talus émergés
     real(DOUBLE)                :: Lambda     ! coefficient d'homothétie pour glissement
											   ! d'une colonne de sédiment (si modele = 2)
     real(DOUBLE)                :: ResRed     ! Coef. de résist. résiduelle (si modele=2)
     real(DOUBLE)                :: GammaW     ! poids volum. de l'eau (si modele = 2)

     real(DOUBLE),dimension(:),pointer :: Gamma  ! Coefficient des Parthéniades
  END TYPE TALUS_T

End Module M_TALUS_T
