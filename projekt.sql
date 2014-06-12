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
  mail text unique check (mail ~ '(([a-z]|[._,-]|\d)+)@(([a-z]|[_,-]|\d)+)\.(com|org|pl|fm)')
);

drop table if exists wlasciciel cascade;
drop sequence if exists wlid_seq cascade;

create sequence wlid_seq;
create table wlasciciel (
  wlid integer primary key default nextval('wlid_seq'),
  mail text unique check (mail ~ '(([a-z]|[._,-]|\d)+)@(([a-z]|[_,-]|\d)+)\.(com|org|pl|fm)'),
  marza decimal(3,2) not null check ( marza < 1 and marza > 0)
);

drop table if exists zamowienie cascade;
drop sequence if exists zaid_seq cascade;

create sequence zaid_seq;
create table zamowienie (
  zaid integer primary key default nextval('zaid_seq'),
  realizacja date null check (realizacja >= zlozenie),
  zlozenie date null,
  wartosc integer not null default 0,
  kuid integer not null references kupujacy,
  wlid integer not null references wlasciciel
);

drop table if exists dostawca cascade;
drop sequence if exists doid_seq cascade;

create sequence doid_seq;
create table dostawca (
  doid integer primary key default nextval('doid_seq'),
  mail text unique check (mail ~ '(([a-z]|[._,-]|\d)+)@(([a-z]|[_,-]|\d)+)\.(com|org|pl|fm)')
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
  cena decimal(9,2) not null,
  constraint dostarczaj_tylko_raz_dany_typ unique (tpid, doid)
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
  if (not exists (
    select * from dostarcza where dostarcza.doid = new.doid and dostarcza.tpid = new.tpid)) then
    raise exception 'ten dostawca nie ma tego produktu';
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

drop function if exists usun_niedostarczany_typ_produktu() cascade;

create function usun_niedostarczany_typ_produktu() returns trigger as $$
begin
  delete from typ_produktu where typ_produktu.tpid = OLD.tpid and 
    not exists ( select * from dostarcza where dostarcza.tpid = OLD.tpid );
  return old;
end
$$ language plpgsql;

create trigger usun_niedostarczany_typ_produktu after delete on dostarcza
  for each row execute procedure usun_niedostarczany_typ_produktu();

drop function if exists wykonaj_zamowienie_jako(int, int) cascade;

create function wykonaj_zamowienie_jako(owner int, ord int) returns setof void as $$
begin
  if ( not exists (select * from zamowienie where zaid = ord and wlid = owner)) then
    raise exception 'to nie jest to zamowienie tego wlasciciela';
  end if;
  if (not exists ( 
    select * from zamowienie where zaid = ord and wlid = owner and zlozenie <= current_date)) then
    raise exception 'nie mozna zrealizowac niezlozonego zamowienia';
  end if;
  update zamowienie set realizacja = current_date;
  return;
end
$$ language plpgsql;


drop function if exists zaloguj_jako_dostawca(int) cascade;
create function zaloguj_jako_dostawca(kid int) returns setof void as $$
begin
  if ( not exists (select * from dostawca where doid = kid )) then
    raise exception 'nie ma takiego dostawcy';
  end if;
  set role 'provider';
  return;
end
$$ language plpgsql;

drop function if exists zaloguj_jako_wlasciciel(int) cascade;
create function zaloguj_jako_wlasciciel(kid int) returns setof void as $$
begin
  if ( not exists (select * from wlasciciel where wlid = kid )) then
    raise exception 'nie ma takiego wlasciciela';
  end if;
  set role 'owner';
  return;
end
$$ language plpgsql;

drop function if exists zaloguj_jako_kupujacy(int) cascade;
create function zaloguj_jako_kupujacy(kid int) returns setof void as $$
begin
  if ( not exists (select * from kupujacy where kuid = kid )) then
    raise exception 'nie ma takiego kupujacego';
  end if;
  set role 'buyer';
  return;
end
$$ language plpgsql;

drop function if exists zloz_zamowienie_jako(int, int) cascade;
create function zloz_zamowienie_jako( buyer int, ord int) returns setof void as $$
begin
  if ( not exists (select * from zamowienie where zaid = ord and kuid = buyer)) then
    raise exception 'to nie jest zamowienie tego kupujacego';
  end if;
  update zamowienie set zlozenie = current_date where zaid = ord;
  return;
end
$$ language plpgsql;

drop function if exists dodaj_do_zamowienia_jako(int,int,int) cascade;
create function dodaj_do_zamowienia_jako(buyer int, ord int, pro int) returns setof void as $$
begin
  if ( not exists (select * from zamowienie where zaid = ord and kuid = buyer )) then
    raise exception 'to nie jest zamowienie tego kupujacego';
  end if;
  if ( not exists ( 
    select * from produkt join zamowienie using (wlid) where prid = pro and zaid = ord )) then
    raise exception 'ten wlasciciel nie ma tego produktu';
  end if;
  update produkt set zaid = ord where prid = pro;
  return;
end
$$ language plpgsql;

drop role if exists owner;
create role owner;
revoke all on produkt, zamowienie, dostarcza, typ_produktu, dostawca, kupujacy, wlasciciel from owner cascade;
grant insert on produkt to owner;
grant update on zamowienie to owner;
grant select on zamowienie, dostarcza, typ_produktu, produkt to owner;
grant select on dostawca, kupujacy to owner;
grant all on zaid_seq,doid_seq,wlid_seq to owner;

drop role if exists buyer;
create role buyer;
revoke all on produkt, zamowienie, dostarcza, typ_produktu, dostawca, kupujacy, wlasciciel from buyer cascade;
grant update on produkt to buyer;
grant insert, update on zamowienie to buyer;
grant select on produkt, typ_produktu,zamowienie to buyer;
grant select on wlasciciel to buyer;
grant all on zaid_seq,doid_seq,wlid_seq to buyer;

drop role if exists provider;
create role provider;
revoke all on produkt, zamowienie, dostarcza, typ_produktu, dostawca, kupujacy, wlasciciel from provider cascade;
grant insert, update on typ_produktu, dostarcza to provider;
grant select on dostarcza, typ_produktu to provider;
grant select on wlasciciel to provider;
grant delete on dostarcza to provider;
grant all on zaid_seq,doid_seq,wlid_seq to provider;
