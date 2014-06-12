--
-- Name: doid_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('doid_seq', 10, true);


--
-- Data for Name: dostawca; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY dostawca (doid, mail) FROM stdin;
1	dostawca4@mail.pl
2	dostawca3@mail.pl
3	dostawca2@mail.pl
4	dostawca1@mail.pl
\.


--
-- Name: kuid_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('kuid_seq', 10, true);


--
-- Data for Name: kupujacy; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY kupujacy (kuid, mail) FROM stdin;
1	ja1@kupujacy.com
2	ja2@kupujacy.com
3	ja3@kupujacy.com
\.


--
-- Name: prid_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('prid_seq', 10, false);


--
-- Data for Name: produkt; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY produkt (prid, tpid, cena, zaid, doid, wlid) FROM stdin;
10	1	112.00	\N	1	1
11	3	25.76	\N	1	1
\.


--
-- Name: tpid_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('tpid_seq', 10, false);


--
-- Data for Name: typ_produktu; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY typ_produktu (tpid, nazwa, opis) FROM stdin with csv;
1,hasz    ,to_jest_produkt_kolekcjonerski
2,koka    ,to_tez
3,lsd     ,to_dla_odmiany_tez
4,doniczka,a_w_tym_je_mozna_trzymac
\.


--
-- Data for Name: wlasciciel; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY wlasciciel (wlid, mail, marza) FROM stdin;
1	wl3@wl.com	0.12
2	wl2@wl.com	0.62
3	wl1@wl.com	0.22
\.


--
-- Name: wlid_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('wlid_seq', 10, true);


--
-- Name: zaid_seq; Type: SEQUENCE SET; Schema: public; Owner: postgres
--

SELECT pg_catalog.setval('zaid_seq', 10, false);


--
-- Data for Name: zamowienie; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY zamowienie (zaid, realizacja, zlozenie, wartosc, kuid, wlid) FROM stdin;
10	\N	\N	0	1	1
\.


--
-- Data for Name: dostarcza; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY dostarcza (tpid, doid, cena) FROM stdin with csv;
1,1,100.00
2,1,10.00
3,1,23.00
4,2,123.00
1,2,109.00
4,3,11.00
2,3,78.00
1,4,93.00
\.


