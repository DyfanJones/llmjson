test_that("repair_json_file parses array with unquoted multi-word values", {
  result <- repair_json_file("invalid.json", return_objects = TRUE)

  expect_equal(
    result,
    list(
      list(
        `_id` = "655b66256574f09bdae8abe8",
        about = "Mollit consectetur excepteur voluptate tempor dolore ullamco enim irure ullamco non enim officia. Voluptate occaecat proident laboris ea Lorem cupidatat reprehenderit nisi nisi aliqua. Amet nulla ipsum deserunt excepteur amet ad aute aute ex. Et enim minim sit veniam est quis dolor nisi sunt quis eiusmod in. Amet eiusmod cillum sunt occaecat dolor laboris voluptate in eiusmod irure aliqua duis.",
        address = "536 Montague Terrace, Jenkinsville, Kentucky, 2235",
        age = 32L,
        balance = "$2,562.78",
        company = "EMPIRICA",
        email = "gloverrivas@empirica.com",
        eyeColor = "brown",
        favoriteFruit = "strawberry",
        friends = list(
          list(id = 0L, name = "Cara Shepherd"),
          list(id = 1L, name = "Mason Farley"),
          list(id = 2L, name = "Harriet Cochran")
        ),
        gender = "male",
        greeting = "Hello, Glover Rivas! You have 7 unread messages.",
        guid = "31082ae3-b0f3-4406-90f4-cc450bd4379d",
        index = 0L,
        isActive = FALSE,
        latitude = 36.26102,
        longitude = -91.304608,
        name = "Glover Rivas",
        phone = "+1 (842) 507-3063",
        picture = "http://placehold.it/32x32",
        registered = "2023-11-18T09:32:36 -01:00",
        tags = c("non", "tempor", "do", "ullamco", "dolore", "sunt", "ipsum")
      ),
      list(
        `_id` = "655b662585364bc57278bb6f",
        about = "Irure proident adipisicing do Lorem do incididunt in laborum in eiusmod eiusmod ad elit proident. Eiusmod dolor ex magna magna occaecat. Nulla deserunt velit ex exercitation et irure sunt. Cupidatat ut excepteur ea quis labore sint cupidatat incididunt amet eu consectetur cillum ipsum proident. Occaecat exercitation aute laborum dolor proident reprehenderit laborum in voluptate culpa. Exercitation nulla adipisicing culpa aute est deserunt ea nisi deserunt consequat occaecat ut et non. Incididunt ex exercitation dolor dolor anim cillum dolore.",
        address = "537 Doone Court, Waiohinu, Michigan, 3215",
        age = 38L,
        balance = "$1,359.48",
        company = "MARQET",
        email = "brandimoreno@marqet.com",
        eyeColor = "brown",
        favoriteFruit = "apple",
        friends = list(
          list(id = 0L, name = "Erna Kelly"),
          list(id = 1L, name = "Black Mays"),
          list(id = 2L, name = "Davis Buck")
        ),
        gender = "female",
        greeting = "Hello, Brandi Moreno! You have 1 unread messages.",
        guid = "0dea7a3a-f812-4dde-b78d-7a9b58e5da05",
        index = 1L,
        isActive = TRUE,
        latitude = -19.768953,
        longitude = 8.948458,
        name = "Brandi Moreno",
        phone = "+1 (850) 434-2077",
        picture = "http://placehold.it/32x32",
        registered = "2015-09-03T11:47:15 -02:00",
        tags = c(
          "laboris",
          "occaecat",
          "laborum",
          "laborum",
          "ex",
          "cillum",
          "occaecat"
        )
      ),
      list(
        `_id` = "655b6625870da431bcf5e0c2",
        about = "Non commodo excepteur nostrud qui adipisicing aliquip dolor minim nulla culpa proident. In ad cupidatat ea mollit ex est do deserunt proident nostrud. Cillum id id eiusmod amet exercitation nostrud cillum sunt deserunt dolore deserunt eiusmod mollit. Ut ex tempor ad laboris voluptate labore id officia fugiat exercitation amet.",
        address = "766 Osborn Street, Bath, Maine, 7666",
        age = 20L,
        balance = "$1,493.77",
        company = "OPTIQUE",
        email = "moodymeadows@optique.com",
        eyeColor = "brown",
        favoriteFruit = "strawberry",
        friends = list(
          list(id = 0L, name = "Lacey Cash"),
          list(id = 1L, name = "Gabrielle Harmon"),
          list(id = 2L, name = "Ellis Lambert")
        ),
        gender = "male",
        greeting = "Hello, Moody Meadows! You have 4 unread messages.",
        guid = "b17f6e3f-c898-4334-abbf-05cf222f143b",
        index = 2L,
        isActive = FALSE,
        latitude = -25.847327,
        longitude = 63.95991,
        name = "Moody Meadows",
        phone = "+1 (993) 566-3041",
        picture = "http://placehold.it/32x32",
        registered = "2015-01-16T02:48:28 -01:00",
        tags = c(
          "aute",
          "commodo",
          "adipisicing",
          "nostrud",
          "duis",
          "mollit",
          "ut"
        )
      ),
      list(
        `_id` = "655b6625f3e1bf422220854e",
        about = "Consequat aliquip irure Lorem cupidatat nulla magna ullamco nulla voluptate adipisicing anim consectetur tempor aliquip. Magna aliqua nulla eu tempor esse proident. Proident fugiat ad ex Lorem reprehenderit dolor aliquip labore labore aliquip. Deserunt aute enim ea minim officia anim culpa sint commodo. Cillum consectetur excepteur aliqua exercitation Lorem veniam voluptate.",
        address = "135 Milton Street, Graniteville, Nebraska, 276",
        age = 22L,
        balance = "$2,215.34",
        company = "BLEENDOT",
        email = "heathnguyen@bleendot.com",
        eyeColor = "brown",
        favoriteFruit = "apple",
        friends = list(
          list(id = 0L, name = "Walker Hernandez"),
          list(id = 1L, name = "Maria Lane"),
          list(id = 2L, name = "Mcknight Barron")
        ),
        gender = "male",
        greeting = "Hello, Heath Nguyen! You have 4 unread messages.",
        guid = "92229883-2bfd-4974-a08c-1b506b372e46",
        index = 3L,
        isActive = FALSE,
        latitude = -60.997048,
        longitude = -102.397885,
        name = "Heath Nguyen",
        phone = "+1 (989) 512-2797",
        picture = "http://placehold.it/32x32",
        registered = "2016-07-06T01:31:07 -02:00",
        tags = c("do", "ad", "consequat", "irure", "tempor", "elit", "minim")
      ),
      list(
        `_id` = "655b6625519a5b5e4b6742bf",
        about = "Laboris eu nulla esse magna sit eu deserunt non est aliqua exercitation commodo. Ad occaecat qui qui laborum dolore anim Lorem. Est qui occaecat irure enim deserunt enim aliqua ex deserunt incididunt esse. Quis in minim laboris proident non mollit. Magna ea do labore commodo. Et elit esse esse occaecat officia ipsum nisi.",
        address = "487 Schaefer Street, Wattsville, West Virginia, 4506",
        age = 33L,
        balance = "$1,358.90",
        company = "OATFARM",
        email = "deidreduke@oatfarm.com",
        eyeColor = "brown",
        favoriteFruit = "apple",
        friends = list(
          list(id = 0L, name = "Bean Paul"),
          list(id = 1L, name = "Cochran Hubbard"),
          list(id = 2L, name = "Rodgers Atkinson")
        ),
        gender = "female",
        greeting = "Hello, Deidre Duke! You have 6 unread messages.",
        guid = "c5dc685f-6d0d-4173-b4cf-f5df29a1e8ef",
        index = 4L,
        isActive = TRUE,
        latitude = 68.609781,
        longitude = -87.509134,
        name = "Deidre Duke",
        phone = "+1 (875) 587-3256",
        picture = "http://placehold.it/32x32",
        registered = "2021-09-12T04:17:08 -02:00",
        tags = c(
          "mollit",
          "cupidatat",
          "irure",
          "sit",
          "consequat",
          "anim",
          "fugiat"
        )
      ),
      list(
        `_id` = "655b6625a19b3f7e5f82f0ea",
        about = "Consectetur ea est labore commodo laborum mollit pariatur non enim. Est dolore et non laboris tempor. Ea incididunt ut adipisicing cillum labore officia tempor eiusmod commodo. Cillum fugiat ex consectetur ut nostrud anim nostrud exercitation ut duis in ea. Eu et id fugiat est duis eiusmod ullamco quis officia minim sint ea nisi in.",
        address = "554 Rockaway Parkway, Kohatk, Montana, 6316",
        age = 26L,
        balance = "$3,554.36",
        company = "ESCENTA",
        email = "lydiaholland@escenta.com",
        eyeColor = "blue",
        favoriteFruit = "banana",
        friends = list(
          list(id = 0L, name = "Debra Massey"),
          list(id = 1L, name = "Weiss Savage"),
          list(id = 2L, name = "Shannon Guerra")
        ),
        gender = "female",
        greeting = "Hello, Lydia Holland! You have 5 unread messages.",
        guid = "75f3c264-baa1-47a0-b21c-4edac23d9935",
        index = 5L,
        isActive = TRUE,
        latitude = -88.495799,
        longitude = 71.840667,
        name = "Lydia Holland",
        phone = "+1 (927) 482-3436",
        picture = "http://placehold.it/32x32",
        registered = "2018-03-13T01:48:56 -01:00",
        tags = c(
          "veniam",
          "minim",
          "consequat",
          "consequat",
          "incididunt",
          "consequat",
          "elit"
        )
      )
    )
  )
})
