defmodule Platform.Repo.Migrations.AddFieldsToPlayerAccounts do
  use Ecto.Migration

  def change do
    alter table(:accounts_players) do
      add :display_name, :string
      add :password, :string
      add :password_hash, :string
    end

  end
end
