-- ============================================================================
-- Posicions dels jugadors + canvis de columnes del scraper
-- Executa-ho a Supabase → SQL Editor (és idempotent: es pot repetir).
-- ============================================================================

-- 1) Taules noves ------------------------------------------------------------
create table if not exists player_positions (
  id          bigserial primary key,
  categoria   text      not null,
  grup        int       not null,
  team        text      not null,
  player      text      not null,
  posicio     text      not null check (posicio in ('POR','DEF','MIG','DAV')),
  confianca   real,
  fiabilitat  text,                 -- alta | mitjana | baixa
  p_por       real,
  p_def       real,
  p_mig       real,
  p_dav       real,
  dorsal      int,
  n_partits   int,
  n_titular   int,
  minuts      int,
  gols        int,
  font        text
);
create index if not exists player_positions_idx on player_positions (categoria, grup, team);

create table if not exists match_roles (
  id              bigserial primary key,
  categoria       text not null,
  grup            int  not null,
  jornada         int  not null,
  home_team       text not null,
  away_team       text not null,
  team            text not null,
  player          text not null,
  dorsal          int,
  ordre           int,
  rol             text not null check (rol in ('POR','DEF','MIG','DAV')),
  formacio        text,
  formacio_marge  real
);
create index if not exists match_roles_idx on match_roles (categoria, grup, jornada);

-- 2) Columnes noves a taules existents ---------------------------------------
alter table matches_lineups add column if not exists ordre int;
alter table matches_lineups add column if not exists minutes_played int;
alter table matches_lineups add column if not exists goals int;
alter table matches_lineups add column if not exists yellow_cards int;
alter table matches_lineups add column if not exists red_cards int;
alter table matches add column if not exists date text;
alter table matches add column if not exists time text;

-- `position` ha de ser text ("Titular"/"Suplent"). Si ja ho és, no fa res.
alter table matches_lineups alter column position type text using position::text;

-- Un partit no jugat NO és 0-0: permet NULL als gols.
alter table matches alter column goals_home drop not null;
alter table matches alter column goals_away drop not null;
alter table matches_info alter column goals_home drop not null;
alter table matches_info alter column goals_away drop not null;

-- 3) Neteja de dades antigues amb "0-0" falsos (partits no jugats) -------------
-- Només si tens la certesa que els partits sense acta/resultat real estan a 0-0:
-- update matches set goals_home = null, goals_away = null
--  where (categoria, grup, jornada, local_team) not in
--        (select categoria, grup, jornada, home_team from matches_info);
-- (No cal si tornes a executar el scraper complet: reescriu cada grup.)

-- 4) Lectura pública (si el frontend/anon key ho necessita) -------------------
-- alter table player_positions enable row level security;
-- create policy "lectura" on player_positions for select using (true);
-- alter table match_roles enable row level security;
-- create policy "lectura" on match_roles for select using (true);

-- 3) Capità (marcat a l'acta amb una "C" al costat del nom) -------------------
alter table matches_lineups add column if not exists capita int default 0;
