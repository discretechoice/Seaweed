###############################################################################
#
#    An empirical comparison of (un)correlated random parameter logit and 
#    hybrid choice models for environmental valuation: which model to use?
#
###############################################################################

# Estimating a random parameter model (with uncorrelated parameters) for England

# An html output file will be created at the end, which will be Table 6 + Table A1

from biogeme import *
from headers import *
from loglikelihood import *
from statistics import *         
 
#  Variable description

#  marital_status:  1   single
#                   2   married
#                   3   separated
#                   4   widowed
#                   5   cohabiting 
#         
#  num_children:  number of children living
#
#  education: 1   no education
#             2   primary
#             3   GCSE
#             4   A levels         
#             5   Bachelor
#             6   master
#             7   PhD
#             8   Foundation degree
#          
#  economic_status:  1   employed full time
#                    2   employed part time
#                    3   self-employed
#                    4   retired
#                    5   unemployed
#                    6   homemaker
#                    7   student
#
#  buy_green_energy: 1   yes
#                    2   no
#                    3   don't know
#
#  ideo: political orientation
#        1 (left) - 10 (right)
#
#  income:  1   < 15.000
#           2   15.000-23.500
#           3   23.501-33.800
#           4   33.801-48.000
#           5   48.001-87.500
#           6   < 87.501
#


# Creating variables for estimation

cohabit =  DefineVariable('cohabit', 0 )        
cohabit = ( cohabit + 1 * (  marital_status == 2 ) + 1 * (  marital_status == 5 )) 

numchild =  DefineVariable('numchild', 0 )        
numchild =  num_children 

higheduc =  DefineVariable('higheduc', 0 )        
higheduc = ( higheduc + 1 * (  education > 4 ) ) 

employed =  DefineVariable('employed', 0 )        
employed = ( employed + 1 * (  economic_status < 4 ) )

green =  DefineVariable('green', 0 )        
green = ( green + 1 * (  buy_green_energy == 1 ) ) 

polorient =  DefineVariable('polorient', 0 )        
polorient =  ideo

highincome =  DefineVariable('highincome', 0 )        
highincome = ( highincome + 1 * (  income > 4 ) )
 
 
# Defining Betas, using starting values from a previous estimation

ASC1 = Beta('ASC1',0.0713428,-100,100,0,'ASC1' )
ASC3 = Beta('ASC3',-3.09694,-100,100,0,'ASC3' )

bbATTR1hh2                 = Beta('bbATTR1hh2'              ,0.729069  ,-100,100,0,'bbATTR1hh2' )
bbATTR1ageEnvhh2           = Beta('bbATTR1ageEnvhh2'        ,0.0119733 ,-100,100,0,'bbATTR1ageEnvhh2    ' )
bbATTR1cohabitEnvhh2       = Beta('bbATTR1cohabitEnvhh2'    ,0.0190487 ,-100,100,0,'bbATTR1cohabitEnvhh2    ' )
bbATTR1employedEnvhh2      = Beta('bbATTR1employedEnvhh2'   ,-0.199049 ,-100,100,0,'bbATTR1employedEnvhh2    ' )
bbATTR1femaleEnvhh2        = Beta('bbATTR1femaleEnvhh2'     ,0.106598  ,-100,100,0,'bbATTR1femaleEnvhh2    ' )
bbATTR1greenEnvhh2         = Beta('bbATTR1greenEnvhh2'      ,-0.0115602,-100,100,0,'bbATTR1greenEnvhh2    ' )
bbATTR1higheducEnvhh2      = Beta('bbATTR1higheducEnvhh2'   ,0.197029  ,-100,100,0,'bbATTR1higheducEnvhh2    ' )
bbATTR1highincomeEnvhh2    = Beta('bbATTR1highincomeEnvhh2' ,0.52682   ,-100,100,0,'bbATTR1highincomeEnvhh2    ' )
bbATTR1numchildEnvhh2      = Beta('bbATTR1numchildEnvhh2'   ,-0.0906181,-100,100,0,'bbATTR1numchildEnvhh2    ' )
bbATTR1polorientEnvhh2     = Beta('bbATTR1polorientEnvhh2'  ,-0.107403 ,-100,100,0,'bbATTR1polorientEnvhh2    ' )
sdbATTR1hh2                = Beta('sdbATTR1hh2'             ,0.369259  ,-100,100,0,'sdbATTR1hh2' )

bbATTR1hh3 = Beta('bbATTR1hh3',0.783743,-100,100,0,'bbATTR1hh3' )
bbATTR1ageEnvhh3     = Beta('bbATTR1ageEnvhh3    ',0.0217374,-100,100,0,'bbATTR1ageEnvhh3    ' )
bbATTR1cohabitEnvhh3     = Beta('bbATTR1cohabitEnvhh3    ',-0.225506,-100,100,0,'bbATTR1cohabitEnvhh3    ' )
bbATTR1employedEnvhh3     = Beta('bbATTR1employedEnvhh3    ',-0.243178,-100,100,0,'bbATTR1employedEnvhh3    ' )
bbATTR1femaleEnvhh3     = Beta('bbATTR1femaleEnvhh3    ',0.219812,-100,100,0,'bbATTR1femaleEnvhh3    ' )
bbATTR1greenEnvhh3     = Beta('bbATTR1greenEnvhh3    ',0.104257,-100,100,0,'bbATTR1greenEnvhh3    ' )
bbATTR1higheducEnvhh3     = Beta('bbATTR1higheducEnvhh3    ',0.268551,-100,100,0,'bbATTR1higheducEnvhh3    ' )
bbATTR1highincomeEnvhh3     = Beta('bbATTR1highincomeEnvhh3    ',0.74068,-100,100,0,'bbATTR1highincomeEnvhh3    ' )
bbATTR1numchildEnvhh3     = Beta('bbATTR1numchildEnvhh3    ',0.0115856,-100,100,0,'bbATTR1numchildEnvhh3    ' )
bbATTR1polorientEnvhh3     = Beta('bbATTR1polorientEnvhh3    ',-0.136358,-100,100,0,'bbATTR1polorientEnvhh3    ' )
sdbATTR1hh3 = Beta('sdbATTR1hh3',1.23476,-100,100,0,'sdbATTR1hh3' )

bbATTR2coast2 = Beta('bbATTR2coast2',0.188374,-100,100,0,'bbATTR2coast2' )
bbATTR2ageEnvcoast2  = Beta('bbATTR2ageEnvcoast2 ',-0.00962202,-100,100,0,'bbATTR2ageEnvcoast2 ' )
bbATTR2cohabitEnvcoast2  = Beta('bbATTR2cohabitEnvcoast2 ',0.11469,-100,100,0,'bbATTR2cohabitEnvcoast2 ' )
bbATTR2employedEnvcoast2  = Beta('bbATTR2employedEnvcoast2 ',-0.0716263,-100,100,0,'bbATTR2employedEnvcoast2 ' )
bbATTR2femaleEnvcoast2  = Beta('bbATTR2femaleEnvcoast2 ',-0.084166,-100,100,0,'bbATTR2femaleEnvcoast2 ' )
bbATTR2greenEnvcoast2  = Beta('bbATTR2greenEnvcoast2 ',0.050517,-100,100,0,'bbATTR2greenEnvcoast2 ' )
bbATTR2higheducEnvcoast2  = Beta('bbATTR2higheducEnvcoast2 ',0.0314538,-100,100,0,'bbATTR2higheducEnvcoast2 ' )
bbATTR2highincomeEnvcoast2  = Beta('bbATTR2highincomeEnvcoast2 ',-0.164022,-100,100,0,'bbATTR2highincomeEnvcoast2 ' )
bbATTR2numchildEnvcoast2  = Beta('bbATTR2numchildEnvcoast2 ',-0.0184085,-100,100,0,'bbATTR2numchildEnvcoast2 ' )
bbATTR2polorientEnvcoast2  = Beta('bbATTR2polorientEnvcoast2 ',0.012144,-100,100,0,'bbATTR2polorientEnvcoast2 ' )
sdbATTR2coast2 = Beta('sdbATTR2coast2',0.781588,-100,100,0,'sdbATTR2coast2' )

bbATTR2coast3 = Beta('bbATTR2coast3',0.0392177,-100,100,0,'bbATTR2coast3' )
bbATTR2ageEnvcoast3  = Beta('bbATTR2ageEnvcoast3 ',-0.00801323,-100,100,0,'bbATTR2ageEnvcoast3 ' )
bbATTR2cohabitEnvcoast3  = Beta('bbATTR2cohabitEnvcoast3 ',0.00104757,-100,100,0,'bbATTR2cohabitEnvcoast3 ' )
bbATTR2employedEnvcoast3  = Beta('bbATTR2employedEnvcoast3 ',-0.0434692,-100,100,0,'bbATTR2employedEnvcoast3 ' )
bbATTR2femaleEnvcoast3  = Beta('bbATTR2femaleEnvcoast3 ',-0.128359,-100,100,0,'bbATTR2femaleEnvcoast3 ' )
bbATTR2greenEnvcoast3  = Beta('bbATTR2greenEnvcoast3 ',-0.144358,-100,100,0,'bbATTR2greenEnvcoast3 ' )
bbATTR2higheducEnvcoast3  = Beta('bbATTR2higheducEnvcoast3 ',-0.0755022,-100,100,0,'bbATTR2higheducEnvcoast3 ' )
bbATTR2highincomeEnvcoast3  = Beta('bbATTR2highincomeEnvcoast3 ',-0.203158,-100,100,0,'bbATTR2highincomeEnvcoast3 ' )
bbATTR2numchildEnvcoast3  = Beta('bbATTR2numchildEnvcoast3 ',0.0655021,-100,100,0,'bbATTR2numchildEnvcoast3 ' )
bbATTR2polorientEnvcoast3  = Beta('bbATTR2polorientEnvcoast3 ',0.00350535,-100,100,0,'bbATTR2polorientEnvcoast3 ' )
sdbATTR2coast3 = Beta('sdbATTR2coast3',1.17148,-100,100,0,'sdbATTR2coast3' )

bbATTR3cost = Beta('bbATTR3cost',-4.7309,-100,100,0,'bbATTR3cost' )
bbATTR3ageEnvcost    = Beta('bbATTR3ageEnvcost   ',0.0398291,-100,100,0,'bbATTR3ageEnvcost   ' )
bbATTR3cohabitEnvcost    = Beta('bbATTR3cohabitEnvcost   ',-0.124984,-100,100,0,'bbATTR3cohabitEnvcost   ' )
bbATTR3employedEnvcost    = Beta('bbATTR3employedEnvcost   ',0.0294803,-100,100,0,'bbATTR3employedEnvcost   ' )
bbATTR3femaleEnvcost    = Beta('bbATTR3femaleEnvcost   ',0.221881,-100,100,0,'bbATTR3femaleEnvcost   ' )
bbATTR3greenEnvcost    = Beta('bbATTR3greenEnvcost   ',-1.14997,-100,100,0,'bbATTR3greenEnvcost   ' )
bbATTR3higheducEnvcost    = Beta('bbATTR3higheducEnvcost   ',0.118948,-100,100,0,'bbATTR3higheducEnvcost   ' )
bbATTR3highincomeEnvcost    = Beta('bbATTR3highincomeEnvcost   ',-0.185087,-100,100,0,'bbATTR3highincomeEnvcost   ' )
bbATTR3numchildEnvcost    = Beta('bbATTR3numchildEnvcost   ',-0.0855123,-100,100,0,'bbATTR3numchildEnvcost   ' )
bbATTR3polorientEnvcost    = Beta('bbATTR3polorientEnvcost   ',-0.028457,-100,100,0,'bbATTR3polorientEnvcost   ' )
sdbATTR3cost = Beta('sdbATTR3cost',1.51069,-100,100,0,'sdbATTR3cost' )

bbATTR4perk1 = Beta('bbATTR4perk1',0.255664,-100,100,0,'bbATTR4perk1' )
bbATTR4ageEnvperk1   = Beta('bbATTR4ageEnvperk1  ',-0.00877241,-100,100,0,'bbATTR4ageEnvperk1  ' )
bbATTR4cohabitEnvperk1   = Beta('bbATTR4cohabitEnvperk1  ',0.0641512,-100,100,0,'bbATTR4cohabitEnvperk1  ' )
bbATTR4employedEnvperk1   = Beta('bbATTR4employedEnvperk1  ',0.200105,-100,100,0,'bbATTR4employedEnvperk1  ' )
bbATTR4femaleEnvperk1   = Beta('bbATTR4femaleEnvperk1  ',-0.0180679,-100,100,0,'bbATTR4femaleEnvperk1  ' )
bbATTR4greenEnvperk1   = Beta('bbATTR4greenEnvperk1  ',-0.140967,-100,100,0,'bbATTR4greenEnvperk1  ' )
bbATTR4higheducEnvperk1   = Beta('bbATTR4higheducEnvperk1  ',-0.0668672,-100,100,0,'bbATTR4higheducEnvperk1  ' )
bbATTR4highincomeEnvperk1   = Beta('bbATTR4highincomeEnvperk1  ',0.036874,-100,100,0,'bbATTR4highincomeEnvperk1  ' )
bbATTR4numchildEnvperk1   = Beta('bbATTR4numchildEnvperk1  ',-0.00938898,-100,100,0,'bbATTR4numchildEnvperk1  ' )
bbATTR4polorientEnvperk1   = Beta('bbATTR4polorientEnvperk1  ',-0.00719174,-100,100,0,'bbATTR4polorientEnvperk1  ' )
sdbATTR4perk1 = Beta('sdbATTR4perk1',0.32667,-100,100,0,'sdbATTR4perk1' )

bbATTR4perk2 = Beta('bbATTR4perk2',-0.460857,-100,100,0,'bbATTR4perk2' )
bbATTR4ageEnvperk2   = Beta('bbATTR4ageEnvperk2  ',-0.0036552,-100,100,0,'bbATTR4ageEnvperk2  ' )
bbATTR4cohabitEnvperk2   = Beta('bbATTR4cohabitEnvperk2  ',-0.12015,-100,100,0,'bbATTR4cohabitEnvperk2  ' )
bbATTR4employedEnvperk2   = Beta('bbATTR4employedEnvperk2  ',0.262567,-100,100,0,'bbATTR4employedEnvperk2  ' )
bbATTR4femaleEnvperk2   = Beta('bbATTR4femaleEnvperk2  ',-0.00637506,-100,100,0,'bbATTR4femaleEnvperk2  ' )
bbATTR4greenEnvperk2   = Beta('bbATTR4greenEnvperk2  ',-0.0409742,-100,100,0,'bbATTR4greenEnvperk2  ' )
bbATTR4higheducEnvperk2   = Beta('bbATTR4higheducEnvperk2  ',-0.0246813,-100,100,0,'bbATTR4higheducEnvperk2  ' )
bbATTR4highincomeEnvperk2   = Beta('bbATTR4highincomeEnvperk2  ',0.216864,-100,100,0,'bbATTR4highincomeEnvperk2  ' )
bbATTR4numchildEnvperk2   = Beta('bbATTR4numchildEnvperk2  ',0.0416292,-100,100,0,'bbATTR4numchildEnvperk2  ' )
bbATTR4polorientEnvperk2   = Beta('bbATTR4polorientEnvperk2  ',0.0372788,-100,100,0,'bbATTR4polorientEnvperk2  ' )
sdbATTR4perk2 = Beta('sdbATTR4perk2',0.564499,-100,100,0,'sdbATTR4perk2' )


# Defining the random variables

R_bbATTR1hh2       =      ( bbATTR1hh2    + bbATTR1ageEnvhh2    * age + bbATTR1cohabitEnvhh2    * cohabit + bbATTR1employedEnvhh2    * employed + bbATTR1femaleEnvhh2    * female + bbATTR1greenEnvhh2    * green + bbATTR1higheducEnvhh2    * higheduc + bbATTR1highincomeEnvhh2    * highincome + bbATTR1numchildEnvhh2    * numchild + bbATTR1polorientEnvhh2    * polorient + sdbATTR1hh2    * bioDraws('RND_ATTR1hh2') )  
R_bbATTR1hh3       =      ( bbATTR1hh3    + bbATTR1ageEnvhh3    * age + bbATTR1cohabitEnvhh3    * cohabit + bbATTR1employedEnvhh3    * employed + bbATTR1femaleEnvhh3    * female + bbATTR1greenEnvhh3    * green + bbATTR1higheducEnvhh3    * higheduc + bbATTR1highincomeEnvhh3    * highincome + bbATTR1numchildEnvhh3    * numchild + bbATTR1polorientEnvhh3    * polorient + sdbATTR1hh3    * bioDraws('RND_ATTR1hh3') )  
R_bbATTR2coast2    =      ( bbATTR2coast2 + bbATTR2ageEnvcoast2 * age + bbATTR2cohabitEnvcoast2 * cohabit + bbATTR2employedEnvcoast2 * employed + bbATTR2femaleEnvcoast2 * female + bbATTR2greenEnvcoast2 * green + bbATTR2higheducEnvcoast2 * higheduc + bbATTR2highincomeEnvcoast2 * highincome + bbATTR2numchildEnvcoast2 * numchild + bbATTR2polorientEnvcoast2 * polorient + sdbATTR2coast2 * bioDraws('RND_ATTR2coast2') )  
R_bbATTR2coast3    =      ( bbATTR2coast3 + bbATTR2ageEnvcoast3 * age + bbATTR2cohabitEnvcoast3 * cohabit + bbATTR2employedEnvcoast3 * employed + bbATTR2femaleEnvcoast3 * female + bbATTR2greenEnvcoast3 * green + bbATTR2higheducEnvcoast3 * higheduc + bbATTR2highincomeEnvcoast3 * highincome + bbATTR2numchildEnvcoast3 * numchild + bbATTR2polorientEnvcoast3 * polorient + sdbATTR2coast3 * bioDraws('RND_ATTR2coast3') )  
R_bbATTR3cost      =  -exp( bbATTR3cost   + bbATTR3ageEnvcost   * age + bbATTR3cohabitEnvcost   * cohabit + bbATTR3employedEnvcost   * employed + bbATTR3femaleEnvcost   * female + bbATTR3greenEnvcost   * green + bbATTR3higheducEnvcost   * higheduc + bbATTR3highincomeEnvcost   * highincome + bbATTR3numchildEnvcost   * numchild + bbATTR3polorientEnvcost   * polorient + sdbATTR3cost   * bioDraws('RND_ATTR3cost') )  
R_bbATTR4perk1     =      ( bbATTR4perk1  + bbATTR4ageEnvperk1  * age + bbATTR4cohabitEnvperk1  * cohabit + bbATTR4employedEnvperk1  * employed + bbATTR4femaleEnvperk1  * female + bbATTR4greenEnvperk1  * green + bbATTR4higheducEnvperk1  * higheduc + bbATTR4highincomeEnvperk1  * highincome + bbATTR4numchildEnvperk1  * numchild + bbATTR4polorientEnvperk1  * polorient + sdbATTR4perk1  * bioDraws('RND_ATTR4perk1') )  
R_bbATTR4perk2     =      ( bbATTR4perk2  + bbATTR4ageEnvperk2  * age + bbATTR4cohabitEnvperk2  * cohabit + bbATTR4employedEnvperk2  * employed + bbATTR4femaleEnvperk2  * female + bbATTR4greenEnvperk2  * green + bbATTR4higheducEnvperk2  * higheduc + bbATTR4highincomeEnvperk2  * highincome + bbATTR4numchildEnvperk2  * numchild + bbATTR4polorientEnvperk2  * polorient + sdbATTR4perk2  * bioDraws('RND_ATTR4perk2') )                                                                      
                                           
                                           
# Defining the utilities                                 
                                           
V1 =  (ASC1                              + 
       R_bbATTR1hh2    * alt1attr1hh2    + 
       R_bbATTR1hh3    * alt1attr1hh3    +
       R_bbATTR2coast2 * alt1attr2coast2 +
       R_bbATTR2coast3 * alt1attr2coast3 +
       R_bbATTR3cost   * alt1attr3cost   +
       R_bbATTR4perk1  * alt1attr4perk1  +
       R_bbATTR4perk2  * alt1attr4perk2  ) 

V2 =  (R_bbATTR1hh2    * alt2attr1hh2    +
       R_bbATTR1hh3    * alt2attr1hh3    +
       R_bbATTR2coast2 * alt2attr2coast2 +
       R_bbATTR2coast3 * alt2attr2coast3 +
       R_bbATTR3cost   * alt2attr3cost   +
       R_bbATTR4perk1  * alt2attr4perk1  +
       R_bbATTR4perk2  * alt2attr4perk2  ) 
         
V3 =  (  ASC3                            +
       R_bbATTR1hh2    * alt3attr1hh2    +
       R_bbATTR1hh3    * alt3attr1hh3    +
       R_bbATTR2coast2 * alt3attr2coast2 +
       R_bbATTR2coast3 * alt3attr2coast3 +
       R_bbATTR3cost   * alt3attr3cost   +
       R_bbATTR4perk1  * alt3attr4perk1  +
       R_bbATTR4perk2  * alt3attr4perk2)          
      
# Associate utility functions with the numbering of alternatives
V = {1: V1,
     2: V2,
     3: V3 }

# Associate the availability conditions with the alternatives
av = {1: one,
      2: one,
      3: one}

# country = 1 -> England
# country = 2 -> NI
# country = 3 -> Scotland

# In python data format, 99999 are missing variables
# Excluding missing variables and outliers
exclude = ( ( country          > 1   )
          + ( Double_id        < 2   ) 
          + ( too_short        < 10  )
          + ( age              < 18  )
          + ( age              > 65  )
          + ( Block            > 100 )  
          + ( env1             > 4   )                    
          + ( env2             > 4   )     
          + ( env3             > 4   )          
          + ( env4             > 4   )           
          + ( env5             > 4   ) 
          + ( env6             > 4   ) 
          + ( env7             > 4   ) 
          + ( pay_elecbill     > 6000)      
          + ( marital_status   > 100 ) 
          + ( num_children     > 10  )
          + ( num_adults       > 6   )
          + ( education        > 100 )
          + ( economic_status  > 100 )
          + ( distance_coast   > 900 )
          + ( buy_green_energy > 100 )
          + ( ideo             > 100 )
          + ( income           > 100 ) 
          + ( ChoiceSum        > 29  )) > 0              

BIOGEME_OBJECT.EXCLUDE = exclude 



# The choice model is a logit, with availability conditions
prob = bioLogit(V,av,Choice)
        
# Iterator on individuals, that is on groups of rows.
metaIterator('personIter','__dataFile__','panelObsIter','ID' )

# For each item of personIter, iterates on the rows of the group. 
rowIterator('panelObsIter','personIter')

#Conditional probability for the sequence of choices of an individual
condProbIndiv = Prod(prob,'panelObsIter')
# 
condLikelihoodOneObs = ( condProbIndiv  )

# Integration by simulation
probIndiv = MonteCarlo(condLikelihoodOneObs)

# Likelihood function
loglikelihood = Sum(log(probIndiv),'personIter')
BIOGEME_OBJECT.ESTIMATE = loglikelihood

BIOGEME_OBJECT.DRAWS = { 'RND_ATTR1hh2'   : ('NORMAL','ID'),
                         'RND_ATTR1hh3'   : ('NORMAL','ID'),
                         'RND_ATTR2coast2': ('NORMAL','ID'),
                         'RND_ATTR2coast3': ('NORMAL','ID'),
                         'RND_ATTR3cost'  : ('NORMAL','ID'),
                         'RND_ATTR4perk1' : ('NORMAL','ID'),
                         'RND_ATTR4perk2' : ('NORMAL','ID')
}                        
                                               
BIOGEME_OBJECT.PARAMETERS['optimizationAlgorithm'] = "CFSQP"
BIOGEME_OBJECT.PARAMETERS['checkDerivatives'] = "0"
BIOGEME_OBJECT.PARAMETERS['numberOfThreads'] = "20"
BIOGEME_OBJECT.PARAMETERS['RandomDistribution'] = "MLHS"
BIOGEME_OBJECT.PARAMETERS['NbrOfDraws'] = "2000"
BIOGEME_OBJECT.PARAMETERS['Seed'] = "17"
