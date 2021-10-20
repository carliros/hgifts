module App where

import Prelude

import Data.Array (index, length, mapWithIndex)
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Halogen (Component, ComponentHTML, HalogenM, Slot, mkEval, mkComponent, defaultEval, modify_)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Intro (_intro, intro)
import Question (QuestionOutput(..), _question, question)
import Results (_results, results)
import Types (QuestionInput)

type AppSlots = (
      question :: forall query. Slot query QuestionOutput Int
    , intro :: forall query. Slot query Void Unit
    , results :: forall query. Slot query Void Unit
    )

type AppState = {
      page :: Int
    , questions :: Array QuestionInput
    , name :: String
    }
data AppAction 
    = Next
    | Previous
    | HandleSelectedOption QuestionOutput

questions = [
    { number: 1, gift: "administrar a", answer: Nothing, question: "Soy capaz de organizar ideas tareas personas y tiempo para el servicio cristiano." }
  , { number: 2, gift: "animar a", answer: Nothing, question: "El Espíritu Santo me ha ayudado a animar a la gente a vivir una verdadera vida cristiana." }
  , { number: 3, gift: "evangelizar a", answer: Nothing, question: "Me gusta hablar de Jesús a los que no le conocen." }
  , { number: 4, gift: "hospedar a", answer: Nothing, question: "Me da gusto proveer alojamiento y no me siento molesto por huéspedes inesperados." }
  , { number: 5, gift: "interceder a", answer: Nothing, question: "Tomo seriamente las peticiones de oración de otros y sigo orando por ellos." }
  , { number: 6, gift: "guiar a", answer: Nothing, question: "Puedo motivar a grupos a alcanzar objetivos bíblicos específicos." }
  , { number: 7, gift: "misericordia a", answer: Nothing, question: "Tengo la habilidad de mostrar compasión por medio de hechos que realizo con amor y bondad." }
  , { number: 8, gift: "predicar a", answer: Nothing, question: "He rogado que la obra de Dios prospere tanto en la iglesia como en el mundo." }
  , { number: 9, gift: "servir a", answer: Nothing, question: "Me gusta hacer cosas que ayuden a otros a servir efectivamente." }
  , { number: 10, gift: "pastorear a", answer: Nothing, question: "He tenido la responsabilidad de animar a otros creyentes en su vida espiritual con buenos resultados." }
  , { number: 11, gift: "enseñar a", answer: Nothing, question: "Entienden bien la lección los alumnos (sean adultos o niños) cuando yo la enseño." }
  , { number: 12, gift: "administrar b", answer: Nothing, question: "Me gusta planear actividades en las cuales otras personas están involucradas." }
  , { number: 13, gift: "animar b", answer: Nothing, question: "Soy sensible a las personas que sufren y que tienen problemas; me gusta ayudarles a ver las respuestas de Dios a los problemas de la vida." }
  , { number: 14, gift: "evangelizar b", answer: Nothing, question: "Me gustaría poder compartir el evangelio libre y eficazmente con personas no creyentes." }
  , { number: 15, gift: "hospedar b", answer: Nothing, question: "Soy sensible a los actos de amabilidad que hacen sentir bien a huéspedes y a extranjeros." }
  , { number: 16, gift: "interceder b", answer: Nothing, question: "Soy sensible a las peticiones de otros y procuro apoyarlos en oración." }
  , { number: 17, gift: "guiar b", answer: Nothing, question: "Tengo el deseo de ayudar dirigir y guiar a personas que desean tener un ministerio importante en la iglesia." }
  , { number: 18, gift: "misericordia b", answer: Nothing, question: "Me gustaría ayudar a los que tienen debilidades mentales y físicas." }
  , { number: 19, gift: "predicar b", answer: Nothing, question: "Tengo un discernimiento espiritual de las Escrituras y sé relacionarlas a personas o cosa. Me gusta compartir o enseñar la Palabra de Dios." }
  , { number: 20, gift: "servir b", answer: Nothing, question: "Soy sensible a las necesidades de otros y estoy dispuesto a darles ayuda." }
  , { number: 21, gift: "pastorear b", answer: Nothing, question: "Estoy preocupado por ver si se llenan las necesidades espirituales de los creyentes. Estoy dispuesto a colaborar en tareas de discipulado y ministerio." }
  , { number: 22, gift: "enseñar b", answer: Nothing, question: "Me gusta ayudar a la gente a entender las cosas de Dios." }
  , { number: 23, gift: "administrar a", answer: Nothing, question: "Soy capaz de hacer planes eficaces para alcanzar metas." }
  , { number: 24, gift: "animar a", answer: Nothing, question: "He animando a la gente de la comunidad cristiana a amar y hacer buenas obras." }
  , { number: 25, gift: "evangelizar a", answer: Nothing, question: "Dios me ha usado para guiar a otros a creer en Cristo como su Salvador." }
  , { number: 26, gift: "hospedar a", answer: Nothing, question: "Tengo la habilidad de ayudar a extranjeros a sentirse como en casa tanto en mi propio hogar como en la iglesia." }
  , { number: 27, gift: "interceder a", answer: Nothing, question: "Oro por otros porque reconozco que la eficacia de su ministerio depende de mis oraciones." }
  , { number: 28, gift: "guiar a", answer: Nothing, question: "Me gusta ayudar a otros a alcanzar sus metas y guiarlos para que crezcan espiritualmente." }
  , { number: 29, gift: "misericordia a", answer: Nothing, question: "Me gusta trabajar con personas que sufren de problemas físicos mentales o emocionales." }
  , { number: 30, gift: "predicar a", answer: Nothing, question: "He proclamado mensajes urgentes y prácticos de la Palabra de Dios." }
  , { number: 31, gift: "servir a", answer: Nothing, question: "Me gusta trabajar en cosas pequeñas para ayudar al desarrollo del cuerpo de Cristo." }
  , { number: 32, gift: "pastorear a", answer: Nothing, question: "Cuando veo una persona descarriada me siento responsable por su bienestar." }
  , { number: 33, gift: "enseñar a", answer: Nothing, question: "Puedo enseñar de una manera sencilla para que tanto los niños como los adultos entiendan la lección." }
  , { number: 34, gift: "administrar b", answer: Nothing, question: "Me gustaría supervisar un programa o ministerio en la iglesia." }
  , { number: 35, gift: "animar b", answer: Nothing, question: "Yo creo que las personas llegarán a la madurez espiritual por medio del consejo e instrucción de la Palabra." }
  , { number: 36, gift: "evangelizar b", answer: Nothing, question: "Siento una carga por amigos y conocidos que no creen en Cristo." }
  , { number: 37, gift: "hospedar b", answer: Nothing, question: "Soy más atento a las necesidades de otros que a las mías." }
  , { number: 38, gift: "interceder b", answer: Nothing, question: "Tengo la convicción de que Dios obra en respuesta a las oraciones y quiero ser usado para ayudar a otros por medio de mis oraciones." }
  , { number: 39, gift: "guiar b", answer: Nothing, question: "Si tuviera la oportunidad me gustaría guiardirigir y motivar a otros en algún aspecto de la obra de Dios." }
  , { number: 40, gift: "misericordia b", answer: Nothing, question: "Cuando miro la miseria humana no me repugna sino más bien me estimula a encontrar la manera de expresar el amor de Dios a los que sufren." }
  , { number: 41, gift: "predicar b", answer: Nothing, question: "Si tuviera oportunidad me gustaría ser un predicador de la Palabra de Dios." }
  , { number: 42, gift: "servir b", answer: Nothing, question: "Me gusta por naturaleza hacer trabajos que ayudan a otros a realizar efectivamente sus propias tareas." }
  , { number: 43, gift: "pastorear b", answer: Nothing, question: "Siento en mí una inquietud pastoral cuando me entero de cristianos que necesitan consejo espiritual." }
  , { number: 44, gift: "enseñar b", answer: Nothing, question: "Reconozco en seguida cuando alguien (niño o adulto) no ha entendido claramente la lección." }
  , { number: 45, gift: "administrar a", answer: Nothing, question: "Poseo el sentido de delegar tareas importantes a las personas indicadas en el tiempo oportuno." }
  , { number: 46, gift: "animar a", answer: Nothing, question: "Estoy feliz cuando las personas que necesitan consuelo consolación ánimo y consejo vienen a buscarme a mí." }
  , { number: 47, gift: "evangelizar a", answer: Nothing, question: "Soy capaz de compartir el evangelio de tal manera que sea claro y significativo para personas no creyentes." }
  , { number: 48, gift: "hospedar a", answer: Nothing, question: "Mi casa siempre está disponible para los que necesitan hospedaje." }
  , { number: 49, gift: "interceder a", answer: Nothing, question: "Soy consciente de ministrar a otros mientras oro por ellos." }
  , { number: 50, gift: "guiar a", answer: Nothing, question: "He aceptado las responsabilidades de un líder y he tenido éxito en guiar al grupo hacia su meta." }
  , { number: 51, gift: "misericordia a", answer: Nothing, question: "Las visitas que hago son de ayuda espiritual a los enfermos inválidos y reclusos." }
  , { number: 52, gift: "predicar a", answer: Nothing, question: "Dios me ha usado para desarrollar animar y fortalecer a otros creyentes hablándoles de la Biblia." }
  , { number: 53, gift: "servir a", answer: Nothing, question: "Encuentro frecuentemente maneras prácticas de ayudar a otros y recibo satisfacción al hacerlo." }
  , { number: 54, gift: "pastorear a", answer: Nothing, question: "El Señor me ha usado para cuidar guiar e instruir a otros creyentes hacia la madurez espiritual." }
  , { number: 55, gift: "enseñar a", answer: Nothing, question: "Puedo captar la atención y el interés de la gente a quien enseño." }
  , { number: 56, gift: "administrar b", answer: Nothing, question: "Soy capaz de saber cómo y cuándo los proyectos y trabajos deben estar mejor organizados." }
  , { number: 57, gift: "animar b", answer: Nothing, question: "Estoy dispuesto a dedicar tiempo cada semana para aconsejar a otros." }
  , { number: 58, gift: "evangelizar b", answer: Nothing, question: "Puedo sentir cuando una persona no conoce a Cristo y eso me da dolor." }
  , { number: 59, gift: "hospedar b", answer: Nothing, question: "Tengo un aprecio genuino para cada huésped en mi casa." }
  , { number: 60, gift: "interceder b", answer: Nothing, question: "Puedo sentir rápidamente cuando un grupo del cual soy miembro está perdiendo el tiempo y quiero hacer algo para cambiarlo." }
  , { number: 61, gift: "guiar b", answer: Nothing, question: "Me sentiría gozoso si alguien me pidiera ser su compañero de oración para su ministerio." }
  , { number: 62, gift: "misericordia b", answer: Nothing, question: "Tengo la habilidad de saber cuando las personas sufren de una manera u otra." }
  , { number: 63, gift: "predicar b", answer: Nothing, question: "Creo que más creyentes deben involucrarse en discusiones y acciones sobre a la moral como el aborto el libertinaje sexual el racismo etc." }
  , { number: 64, gift: "servir b", answer: Nothing, question: "Me gustaría tener más oportunidad de ayudar a otros en sus ministerios." }
  , { number: 65, gift: "pastorear b", answer: Nothing, question: "Me gustaría poder capacitar a los creyentes para la obra en el ministerio." }
  , { number: 66, gift: "enseñar b", answer: Nothing, question: "Me emociona aumentar mis conocimientos de la Palabra de Dios." }
]

app :: forall query input output m. MonadEffect m => Component query input output m
app =
  mkComponent
    { initialState
    , render
    , eval: mkEval $ defaultEval
        { handleAction = handleAction }
    }
  where
  initialState :: input -> AppState
  initialState _ = {
        page: 0
      , questions: questions
      , name: ""
    }

  render :: AppState -> ComponentHTML AppAction AppSlots m
  render { page, questions, name } =
    let bodyContent = if page == 0
                        then HH.div_ [ HH.slot_ _intro unit intro { name: name }]
                        else if page >= 1 && page <= length questions
                                then let maybeQuestionInput = index questions (page - 1)
                                     in case maybeQuestionInput of
                                            Just questionInput -> HH.div_ [ HH.slot _question page question questionInput HandleSelectedOption ]
                                            Nothing -> HH.text "Error"
                                else HH.div_ [ HH.slot_ _results unit results { name: name, questions: questions, page: page }]
    in HH.div [HP.classes [ HH.ClassName "flex-column", HH.ClassName "justify-center", HH.ClassName "h-screen" ] ] [
          HH.h2 [HP.classes [ HH.ClassName "text-2xl" ]] [HH.text "Test de dones espirituales"]
        , bodyContent
        , HH.br_
        , HH.div [] [
              HH.button [ HE.onClick \_ -> Previous, HP.disabled (page == 0)] [ HH.text "Anterior" ]
            , HH.button [ HE.onClick \_ -> Next, HP.disabled (page == (1 + length questions)) ] [ HH.text "Siguiente" ]]
        ]

  handleAction :: AppAction -> HalogenM AppState AppAction AppSlots output m Unit
  handleAction = case _ of
    Next -> modify_ \st -> st { page = st.page + 1 }
    Previous -> modify_ \st -> st { page = st.page - 1 }
    HandleSelectedOption (SelectedOption nro value) -> do
        modify_ \st -> let newQuestions = mapWithIndex (\_ q -> if nro == q.number then q {answer = Just value} else q) st.questions
                       in st { questions = newQuestions, page = st.page + 1 }

{-
<div class="items-center justify-center h-screen flex-column">
      <div class="p-20 text-center">
        <h1 class="text-9xl">tailwindcss 2.0</h1>
        <div class="p-10 text-4xl bg-green-200 hover:bg-green-500">Is now live on server !!</div>
        <div class="p-5 text-2xl">
          <a href="#" class="text-blue-500 hover:text-purple-700">How to set up this tailwind CSS 2.0 ?</a>
        </div>
      </div>
    </div>
-}
