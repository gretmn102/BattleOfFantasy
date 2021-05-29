namespace Shared

type UserId = string

type LoginError =
    | YouAlreadyLogin
    | PlayersRecruited
type GetStateResult<'GameResponse, 'ClientGameState> =
    | WaitPlayers of int
    | PlayerJoined of UserId
    | PlayerLeaved of UserId
    | GameStarted of 'ClientGameState
    | GameResponse of 'GameResponse

type GetStateError =
    | YouAreNotLogin

type SelectAttributeError =
    | TwoEqualAttributes
    | YouDontHaveThisAttribute
    | YouHaveAlreadySelectedAttribute
    | YouDontPlay

type MoveError =
    | GetStateError of GetStateError
    | GameHasNotStartedYet
    | GameEndedError
    | NotYourMove
    | MovError of SelectAttributeError
    | StrError of string

type PlayerId = string

type AttributeId = int
type CharacterId = int


type ThreeCards =
    {
        Attribute1: AttributeId
        Attribute2: AttributeId
        Character: CharacterId
    }
type StartHand =
    {
        Attributes: AttributeId list
        Characters: CharacterId list
    }

type ChoosenHand =
    {
        RetainedAttribute: AttributeId
        RetainedCharacter: CharacterId
        GivenAttribute: AttributeId
    }

type PlayerStage =
    | StartHand of StartHand
    | ThreeCards of ThreeCards
    | ChoosenAttribute of ChoosenHand
    | FinalHand of ThreeCards

module Client =
    type OtherPlayerStage =
        | StartHand
        | ThreeCards
        | ChoosenAttribute
        | FinalHand of ThreeCards

    type MoveStage =
        | SelectThreeCardsStage
        | SelectAttributeStage
        | StartGameStage

    type GameState =
        {
            AttributesDeckCount: int
            CharactersDeckCount: int

            OtherPlayers: Map<UserId, OtherPlayerStage>

            ClientPlayer: PlayerStage

            MoveStage: MoveStage
        }

type GameResponse =
    | SelectedThreeCards of PlayerId
    | StartSelectAttribute
    | SelectedOneAttribute of PlayerId
    | StartGame of (PlayerId * ThreeCards) list

module Init =
    type Attribute =
        {
            Id: AttributeId
            Name: string
        }
    type Character =
        {
            Id: AttributeId
            Name: string
        }
    let attributes =
        [
            "Шестирукий"
            "Мыло — его слабость"
            "С дубиной"
            "Вооружен безопасными детскими ножницами"
            "Очень богатый"
            "Продажный"
            "Младенец"
            "Едет на одноколесном велосипеде"
            "Невесомый"
            "Серьезный"

            "Всеми любимый"
            "Умеет создавать вакуум"
            "Все его преимущества исчезают, остаются лишь недостатки" // TODO
            "Этого персонажа целая армия"
            "Вызывает землетрясения"
            "Стометровый"
            "Пьяный"
            "Чернобыльский"
            "Пахнет как носки бати"
            "Цельнометаллический"

            "Сделано в России"
            "Лысый"
            "Руки-базуки"
            "Голый"
            "Очень хочет в туалет, но пока держится"
            "Боится темноты"
            "Становится невидимым, когда чешет зад двумя руками"
            "Игрушечный"
            "Сделано в Китае"
            "Сладкий"

            "Весит как Юпитер"
            "Имеет 10 миллионов подписчиков на YouTube"
            "Единственный во вселенной"
            "Взрывает предметы силой мысли"
            "Сырный"
            // "Замени эту карту на случайную из колоды атрибутов (эффект срабатывает при разыгрывании)" // TODO
            "Может дышать под водой"
            "Умеет летать"
            "Только что из душа"
            "Помнит имена всех актеров, игравших Могучих Рейнджеров"

            "Падает в обморок при виде Чебупелей"
            "Не различает цвета"
            "Выживает при любой температуре"
            "Его главный страх оживает и нападает на противника"
            "Говорит стихами"
            "Из каменного века"
            "Слепой, но свойства одной другой карты атрибута удваиваются (если карт две или более, противник выбирает одну из них)" // TODO
            "Из будущего"
            "В стиле аниме"
            "Не пьянеет"

            "Демон из параллельного измерения"
            "Может уничтожить весь мир, но пока не хочет"
            "Вернувшийся с того света"
            "Плюется огнем"
            "Прекрасно готовит борщ"
            "Знает главную тайну персонажа противника"
            "Нищий"
            "Играет в рок-группе"
            "Инопланетный"
            "Замораживающее прикосновение"

            "Вечно голодный"
            // "Зовет на помощь друга (вытащи случайную карту персонажа из колоды и добавь в битву)" // TODO
            // "Замени эту карту на две случайные карты атрибута из колоды (эффект срабатывает при разыгрывании)" // TODO
            "Бессмертный"
            "Собирает марки"
            "С ножом"
            "В депрессии"
            "Знает кунг фу"
            "Резиновый"

            // "В теле... (Вытяни карту персонажа)" // TODO
            "Светится в темноте"
            "Водит маршрутку"
            "Заперт в клетке"
            "Ядовитый"
            "В рыцарской броне"
            "Спит"
            "Заботливый"
            "Танцует без остановки, пока играет музыка"
            "В танке"

            "С пистолетом Макарова, в котором осталось два патрона"
        ]
        |> List.indexed

    let characters =
        [
            "Мент из русского сериала"
            "Бабушка"
            "Андроид"
            "Батя"
            "Крыса"
            "Ельцын"
            "Гопник"
            "Пушкин"
            "Буратино"
            "Сын маминой подруги"

            "Ленин"
            "Горшок"
            "Снежная королева"
            "Феникс"
            "Псевдоаниматор"
            "Гоголь"
            "Мойдодыр"
            "Колобок"
            "Школьник"
            "Бендер"

            "Огр"
            // "Замени эту карту на случайную из колоды (эффект срабатывает при разыгрывании)" // TODO
            "Баба-Яга"
            "Кощей Бессмертный"
            "Сын депутата"
            "Америка"
            "#яжемать"
            "Федеральная налоговая служба"
            "Беларусь"
            "Дед Мороз"

            "Алхимик"
            "Русский рэпер"
            "Задорнов"
            "Блогер"
            "Магнит"
            "Гагарин"
            "Вахтерша"
            "Мультяшка"
            "Неисправимый лгун"
            "Ким"

            "КГБ"
            "Бомж"
            "Русский пират"
            // "Замени эту карту на случайную из руки оппонента (эффект срабатывает при разыгрывании)" // TODO
            "Критик"
            "Подлиза"
            "Адвокат"
            "Твоя мама"
            "Лупа и Пупа"
            "Стримерша"
        ]
        |> List.indexed
type Color =
    | Red
    | Green
    | Blue
    | Black

type User = { Name : string; Color: Color }
type Message = { Time : System.DateTime; Content: string }

type Msgs =
    | ClientMsg of string * Message
    | SysMsg of Message

type RemoteClientMsg =
    | QueryConnected
    | GetUsers of User list

    | AddUser of User
    | RemoveUser of string
    | AddMsg of Msgs
    | AddMsgs of Msgs list

    | LoginResult of Result<Client.GameState option, LoginError>

    | GameMsgs of GetStateResult<GameResponse, Client.GameState> list
    | MoveResult of Result<unit, MoveError>

type RemoteServerMsg =
    | SetUser of User
    | SendMsg of string
    | UsersConnected

    | ThreeCardsMove of ThreeCards
    | SelectOneAttributeMove of AttributeId
module Remote =
    let socketPath = "/socket"