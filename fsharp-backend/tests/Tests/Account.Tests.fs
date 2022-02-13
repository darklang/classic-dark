module Tests.Account

// Tests for LibBackend.Account

open Expecto
open Prelude
open TestUtils.TestUtils

module Account = LibBackend.Account

let testAuthentication =
  testTask "authenticated users" {
    let! username = Account.authenticate "test" "fVm2CUePzGKCwoEQQdNJktUQ"
    Expect.equal username (Some "test") "valid authentication"

    let! username = Account.authenticate "test_unhashed" "fVm2CUePzGKCwoEQQdNJktUQ"
    Expect.equal username None "invalid authentication"

    let! username = Account.authenticate "test" "no"
    Expect.equal username None "incorrect hash"

    let! username = Account.authenticate "test_unhashed" "no"
    Expect.equal username None "invalid authentication for unhashed"
  }


let testEmailValidationWorks =
  testMany
    "validateEmail"
    Account.validateEmail
    [ "novalidemail", (Error "Invalid email 'novalidemail'") ]


let testUsernameValidationWorks =
  testMany
    "validateUsername"
    UserName.validate
    [ "Upper",
      (Error
        "Invalid username 'Upper', can only contain lowercase roman letters and digits, or '_'")
      "uPPer",
      (Error
        "Invalid username 'uPPer', can only contain lowercase roman letters and digits, or '_'")
      "a",
      (Error
        "Invalid username 'a', can only contain lowercase roman letters and digits, or '_'")
      "aaa❤️",
      (Error
        "Invalid username 'aaa❤️', can only contain lowercase roman letters and digits, or '_'")
      "aaa-aaa",
      (Error
        "Invalid username 'aaa-aaa', can only contain lowercase roman letters and digits, or '_'")
      "aaa aaa",
      (Error
        "Invalid username 'aaa aaa', can only contain lowercase roman letters and digits, or '_'")
      "aaa_aaa", Ok "aaa_aaa"
      "myusername09", Ok "myusername09"
      "paul", Ok "paul" ]

let testCannotCreateBannedUser =
  let bannedAccount () : Account.Account =
    { username = UserName.create "admin"
      password = LibBackend.Password.invalid
      email = $"test+cannot-create-banned@darklang.com"
      name = "test account" }
  let okAccount (suffix : string) : Account.Account =
    { username = UserName.create $"notbanned_{suffix}"
      password = LibBackend.Password.invalid
      email = $"test+notbanned_{suffix}@darklang.com"
      name = "test account" }
  testList
    "bannedUser"
    [ testTask "upsert banned" {
        let a = bannedAccount ()
        let! upserted = Account.upsertAccount false a
        Expect.equal upserted (Error "Username is not allowed") "banned"
      }
      testTask "upsert not banned" {
        let a = okAccount "a"
        let! upserted = Account.upsertAccount false a
        Expect.equal upserted (Ok()) "not banned"
      }
      testTask "insert banned" {
        let a = bannedAccount ()
        let! inserted = Account.insertUser a.username a.email a.name None
        Expect.equal inserted (Error "Username is not allowed") "banned"
      }
      testTask "insert not banned" {
        let a = okAccount "b"
        let! inserted = Account.insertUser a.username a.email a.name None
        Expect.equal inserted (Ok()) "not banned"
      } ]


let tests =
  testList
    "Account"
    [ testEmailValidationWorks
      testUsernameValidationWorks
      testAuthentication
      testCannotCreateBannedUser ]
