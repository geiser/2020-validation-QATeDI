wants <- c('readr', 'dplyr', 'readxl')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(readr)
library(dplyr)
library(readxl)

options(stringsAsFactors = FALSE)

predat_1st <- read_excel("data/Primeira-Aplicacao.xlsx")  
predat_2nd <- read_excel("data/Segunda-Aplicacao.xlsx")

(colnames(predat_1st)) # dados da primeira aplicação serão ignorados, os itens não correspondem com os indicados na avaliação da qualidade de acesso
(colnames(predat_2nd))

# anonimizando dados
pdat <- data.frame(
  UserID=as.vector(sapply(predat_2nd$`Endereço de e-mail`, FUN = digest::digest))
  # caracterização dos respondentes
  , Unidade=predat_2nd$`Unidade Acadêmica`
  , Nivel=predat_2nd$`Que nível você está cursando?`
  , Modalidade=predat_2nd$`Qual a modalidade de seu curso?`
  , DispositivoEmCasa=recode(predat_2nd$`Eles são compartilhados com outras pessoas?`
                             ,  'Sim' = 'Dispositivo Compartilhado'
                             ,  'Não' = 'Dispositivo Próprio'
                             , .missing = 'Não Tem Nemhum Dispositivo')
  # condição da infraestrutura em casa
  , Item8=predat_2nd$`Você tem acesso à internet na sua casa?`
  , Item9=predat_2nd$`Qual a qualidade do acesso à internet na sua casa?`
  , Item10=predat_2nd$`Como é a disponibilidade do acesso à internet em sua casa?`
  # condição da infraestrutura móvel
  , Item11=predat_2nd$`Você tem acesso à internet em seu celular/smartphone?`
  , Item12=predat_2nd$`Qual a qualidade do acesso à internet em seu celular/smartphone?`
  , Item13=predat_2nd$`Como é a disponibilidade do acesso à internet em seu celular/smartphone?`
  # condição da infraestrutura no trabalho e centro educativo
  , Item14=predat_2nd$`Você tem acesso à internet em seu trabalho?`
  , Item15=predat_2nd$`Qual a qualidade do acesso à internet em seu trabalho ?`
  , Item16=predat_2nd$`Como é a disponibilidade do acesso à internet em seu trabalho?`
  
  , Item17=predat_2nd$`Você tem acesso à internet na UFAL?`
  , Item18=predat_2nd$`Qual a qualidade do acesso à internet na UFAL?`
  , Item19=predat_2nd$`Como é a disponibilidade do acesso à internet na UFAL?`
  # experiência com tecnologias digitais
  , Item20=predat_2nd$`Qual o seu nível de experiência como aluno de educação a distância ?`
  , Item21=predat_2nd$`Qual o seu nível de experiência com videoaulas?`
  , Item22=predat_2nd$`Como é a sua experiência com videoconferência (ou webconferência) com até três pessoas?`
  , Item23=predat_2nd$`Como é a sua experiência com reuniões por videoconferência (ou webconferência) com a presença de mais de três pessoas?`
  , Item24=predat_2nd$`Como é a sua experiência com o Ambiente Virtual de Aprendizagem da UFAL (Moodle)?`
)

pdat <- pdat[!duplicated(pdat$UserID),] # removendo respostas de usuários duplicados
write_csv(pdat, 'data/raw-data.csv')

# definindo grupos de usuários
gdat <- select(pdat, -starts_with('Item'))
write_csv(gdat, 'data/user-groups.csv')

# estabelecendo escalas nos itens referidos aos fatores latentes avaliados pelo questionario
# quando um item é condicionado o valor 0 é adicionado na escala usando a propriedade .missing
rdat <- transmute(
  pdat
  , UserID=UserID
  # factor 1
  , Item8=recode(Item8, 'Sim' = 1, 'Não' = 0)
  , Item9=recode(Item9, 'Excelente' = 5, 'Boa' = 4, 'Regular' = 3, 'Péssima' = 2, 'Ruim' = 1, .missing = 0)
  , Item10=recode(Item10
                  , 'Tenho acesso sempre que eu quero (acesso irrestrito) à internet.' = 5
                  , 'Tenho algumas dificuldades de acesso porque preciso compartilhar com familiares, ou o computador, ou o próprio roteador' = 4
                  , 'Regular. Atende às minhas necessidades.' = 3
                  , 'A internet da minha casa é ruim, mesmo quando estou acessando sozinho' = 2
                  , 'A internet da minha casa é muito precária' = 1
                  , .missing = 0)
  # factor 2
  , Item11=recode(Item11, 'Sim' = 1, 'Não não possuo celular/smartphone' = 0)
  , Item12=recode(Item12, 'Excelente' = 5, 'Boa' = 4, 'Regular' = 3, 'Péssima' = 2, 'Ruim' = 1, .missing = 0)
  , Item13=recode(Item13
                  , 'Tenho acesso sempre que eu quero (acesso irrestrito) à internet' = 5
                  , 'Tenho algumas dificuldades de acesso porque preciso compartilhar com familiares, ou o celular/smartphone, ou o próprio roteador' = 4
                  , 'Regular. Atende às minhas necessidades' = 3
                  , 'A internet do meu celular/smartphone é ruim, mesmo quando estou acessando sozinho' = 2
                  , 'A internet do meu celular/smartphone é muito precária' = 1
                  , .missing = 0)
  # factor 3
  , Item14=recode(Item14, 'Sim' = 1, "Não ou não trabalho" = 0)
  , Item15=recode(Item15, 'Excelente' = 5, 'Boa' = 4, 'Regular' = 3, 'Péssima' = 2, 'Ruim' = 1, .missing = 0)
  , Item16=recode(Item16
                  , 'Tenho acesso sempre que eu quero (acesso irrestrito) à internet' = 5
                  , 'Tenho algumas dificuldades de acesso porque preciso compartilhar com outras pessoas, ou o computador, ou o próprio roteador' = 4
                  , 'Regular. Atende às minhas necessidades' = 3
                  , 'A internet do meu trabalho é ruim, mesmo quando estou acessando sozinho' = 2
                  , 'A internet do meu trabalho é muito precária' = 1
                  , .missing = 0)
  # factor 4
  , Item17=recode(Item17, 'Sim' = 1, 'Não ou nunca usei (recém ingresso na UFAL)' = 0)
  , Item18=recode(Item18, 'Excelente' = 5, 'Boa' = 4, 'Regular' = 3, 'Péssima' = 2, 'Ruim' = 1, .missing = 0)
  , Item19=recode(Item19
                  , 'Tenho acesso sempre que eu quero (acesso irrestrito) à internet' = 5
                  , 'Os laboratórios da universidade suprem a minha necessidade' = 4
                  , 'O acesso à internet na universidade é indiferente para as minhas necessidades' = 3
                  , 'Eu dependo e tenho limitações no acesso à internet da universidade' = 2
                  , 'Tenho acesso precário à internet quando estou na universidade' = 1
                  , .missing = 0)
  # factor 5
  , Item20=recode(Item20
                  , 'Sinto-me muito confortável em estudar na modalidade a distância' = 4
                  , 'Sinto-me muito confortável em estudar na modalidade à distância' = 4
                  , 'Tenho um bom rendimento nessa modalidade, mas prefiro o ensino presencial' = 3
                  , 'Sou indiferente' = 2
                  , 'Tenho muita dificuldade em aprender por meio da educação à distância' = 1
                  , 'Nunca estudei na modalidade à distância' = 0)
  , Item21=recode(Item21
                  , 'Sinto-me muito confortável em assistir à videoaulas' = 4
                  , 'Quando necessário, eu assisto à videoaulas e tenho um bom rendimento de aprendizagem' = 3
                  , 'Sou indiferente' = 2
                  , 'Tenho muito dificuldade em aprender através de videoaulas' = 1
                  , 'Muito raramente assisto à aulas por meio de videoaulas' = 0)
  # factor 6
  , Item22=recode(Item22
                  , 'Eu participo de videoconferências com áudio e vídeo bem nítidos' = 4
                  , 'Quando necessário, eu participo de videoconferência, mas tenho problemas com o áudio ou vídeo (por exemplo, o vídeo ou o áudio costuma ficar cortando)' = 3
                  , 'Sou indiferente ou raramente participo de videoconferência' = 2
                  , 'Com as minhas possibilidades de acesso à internet, não consigo fazer videoconferência' = 1
                  , 'Não gosto de fazer videoconferência' = 0)
  , Item23=recode(Item23
                  , "Eu utilizo videoconferência para fazer reuniões com bastante frequência" = 4
                  , "Quando necessário, eu utilizo, com algum nível de precariedade, videoconferência para fazer reuniões" = 3
                  , "Sou indiferente ou raramente faço reuniões por vídeo conferência" = 2
                  , "Com as minhas possibilidades de acesso à internet, não consigo fazer reunião por videoconferência" = 1
                  , "Não gosto de fazer videoconferência" = 0)
  , Item24=recode(Item24
                  , "Satisfatória, consigo realizar as atividades e aprender" = 4
                  , "Boa, mas sinto dificuldade em algumas atividades" = 3
                  , "Sou indiferente" = 2
                  , "Poucas vezes usei o Moodle da Ufal, e não gostei da experiência" = 1
                  , "Nunca usei o Moodle da Ufal" = 0)
)
write_csv(rdat, 'data/data-withcareless.csv')
