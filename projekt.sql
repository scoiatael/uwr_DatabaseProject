drop table if exists typ_produktu cascade;
drop sequence if exists tpid_seq cascade;

create sequence tpid_seq;
create table typ_produktu (
  tpid integer primary key default nextval('tpid_seq'),
  nazwa text,
  opis text
);

drop table if exists kupujacy cascade;
drop sequence if exists kuid_seq cascade;

create sequence kuid_seq;
create table kupujacy (
  kuid integer primary key default nextval('kuid_seq'),
  mail text check (mail ~ '([a-z]|[._,-])+@[a-z]+\.(com|org|pl|fm)')
);

drop table if exists zamowienie cascade;
drop sequence if exists zaid_seq cascade;

create sequence zaid_seq;
create table zamowienie (
  zaid integer primary key default nextval('zaid_seq'),
  realizacja date null check (realizacja > zlozenie),
  zlozenie date null,
  wartosc integer not null default 0,
  kuid integer not null references kupujacy
);

drop table if exists dostawca cascade;
drop sequence if exists doid_seq cascade;

create sequence doid_seq;
create table dostawca (
  doid integer primary key default nextval('doid_seq'),
  mail text check (mail ~ '([a-z]|[._,-])+@[a-z]+\.(com|org|pl|fm)')
);

drop table if exists wlasciciel cascade;
drop sequence if exists wlid_seq cascade;

create sequence wlid_seq;
create table wlasciciel (
  wlid integer primary key default nextval('wlid_seq'),
  mail text check (mail ~ '([a-z]|[._,-])+@[a-z]+\.(com|org|pl|fm)'),
  marza decimal(3,2) not null check ( marza < 1 and marza > 0)
);

drop table if exists produkt cascade;
drop sequence if exists prid_seq cascade;

create sequence prid_seq;
create table produkt (
  prid integer primary key default nextval('prid_seq'),
  tpid integer not null references typ_produktu,
  cena decimal(6,2) not null,
  zaid integer null references zamowienie,
  doid integer not null references dostawca,
  wlid integer not null references wlasciciel
);

drop table if exists dostarcza cascade;

create table dostarcza (
  tpid integer not null references typ_produktu,
  doid integer not null references dostawca,
  cena decimal(9,2) not null
);

drop function if exists zwieksz_wartosc_produktu_i_zamowienia() cascade;

create function zwieksz_wartosc_produktu_i_zamowienia() returns trigger as $$
begin
  if (TG_OP = 'UPDATE' and ( old.zaid is not null or new.wlid != old.wlid or new.doid != old.doid )) then
    raise exception 'nie mozna zmienic produktu';
  end if;
  if (exists (select * from zamowienie where zamowienie.zaid = new.zaid and zlozenie is not null)) then
    raise exception 'nie mozna dodac do zlozonego zamowienia';
  end if;
  if (new.zaid is not null and not exists (select * from zamowienie where zamowienie.zaid = new.zaid)) then
    raise exception 'nie mozna dodac do nieistniejacego zamowienia';
  end if;
  select into new.cena (1.00 + Y.marza) * X.cena from (select cena from dostarcza where dostarcza.tpid = new.tpid and dostarcza.doid = new.doid) X
    cross join (select marza from wlasciciel where wlasciciel.wlid = new.wlid) Y;
  if (new.zaid is not null) then
    update zamowienie set wartosc = wartosc + new.cena where zamowienie.zaid = new.zaid;
  end if;
  return new;
end
$$ language plpgsql;

create trigger zwieksz_wartosc_produktu_i_zamowienia before insert or update on produkt
  for each row execute procedure zwieksz_wartosc_produktu_i_zamowienia();


drop role if exists owner;
create role owner;
revoke all on produkt, zamowienie, dostarcza, typ_produktu, dostawca, kupujacy, wlasciciel from owner cascade;
grant insert on produkt to owner;
grant update on zamowienie to owner;
grant select on zamowienie, dostarcza, typ_produktu, produkt to owner;
grant select on dostawca, kupujacy to owner;

drop role if exists buyer;
create role buyer;
revoke all on produkt, zamowienie, dostarcza, typ_produktu, dostawca, kupujacy, wlasciciel from buyer cascade;
grant update on produkt to buyer;
grant insert, update on zamowienie to buyer;
grant select on produkt, typ_produktu to buyer;
grant select on wlasciciel to buyer;

drop role if exists provider;
create role provider;
revoke all on produkt, zamowienie, dostarcza, typ_produktu, dostawca, kupujacy, wlasciciel from provider cascade;
grant insert, update on typ_produktu, dostarcza to provider;
grant select on dostarcza, typ_produktu to provider;
--grant select on wlasciciel to provider;
grant delete on dostarcza to provider;
