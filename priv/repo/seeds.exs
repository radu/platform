# Script for populating the database. You can run it as:
#
#     mix run priv/repo/seeds.exs
#
# Inside the script, you can read and write to any of your
# repositories directly:
#
#     Platform.Repo.insert!(%Platform.SomeSchema{})
#
# We recommend using the bang functions (`insert!`, `update!`
# and so on) as they will fail if something goes wrong.

Platform.Repo.insert! (%Platform.Accounts.Player{display_name: "Bob Marley", username: "bob", password_hash: "$2b$12$5Yl7Eg2KEpPlxB345L1AI.ReWqOH1S5XtUa7ACjnuUYw1KkAMMjaO", score: 10000})
Platform.Repo.insert! (%Platform.Accounts.Player{display_name: "Manu Chao", username: "manu", password_hash: "$2b$12$bE7CxuMpd0HgYRoBq.9x8Oc2.XgIarOhmjXPZZ8fDM7kB.WH1zNZS", score: 300})
Platform.Repo.insert! (%Platform.Accounts.Player{display_name: "Joey Arms", username: "joearms", password_hash: "$2b$12$dU06PrPidkLWJfnwLWUDSuXcosVtvtYe4OcoqChxCWtkQRllmoOxG", score: 100})

# games
#
Platform.Repo.insert! (%Platform.Products.Game{title: "Adventure Game", description: "Adventure Game Example", author_id: 1})
Platform.Repo.insert! (%Platform.Products.Game{title: "Driving Game", description: "Driving Game Example", author_id: 2})
Platform.Repo.insert! (%Platform.Products.Game{title: "Platform Game", description: "Platform Game Example", author_id: 3})

