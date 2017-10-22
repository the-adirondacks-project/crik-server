CREATE TABLE video_files (
    id integer NOT NULL,
    url text NOT NULL,
    video_id integer NOT NULL,
    video_lirary_id NOT NULL,
    storage_id text NOT NULL
);

CREATE SEQUENCE video_files_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE video_files_id_seq OWNED BY video_files.id;

CREATE TABLE video_libraries (
    id integer NOT NULL,
    url text NOT NULL
);

CREATE SEQUENCE video_libraries_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE video_libraries_id_seq OWNED BY video_libraries.id;

CREATE TABLE videos (
    id integer NOT NULL,
    name text NOT NULL
);

CREATE SEQUENCE videos_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;

ALTER SEQUENCE videos_id_seq OWNED BY videos.id;

ALTER TABLE ONLY video_files ALTER COLUMN id SET DEFAULT nextval('video_files_id_seq'::regclass);

ALTER TABLE ONLY video_libraries ALTER COLUMN id SET DEFAULT nextval('video_libraries_id_seq'::regclass);

ALTER TABLE ONLY videos ALTER COLUMN id SET DEFAULT nextval('videos_id_seq'::regclass);

ALTER TABLE ONLY video_files
    ADD CONSTRAINT video_files_pkey PRIMARY KEY (id);

ALTER TABLE ONLY video_libraries
    ADD CONSTRAINT video_libraries_pkey PRIMARY KEY (id);

ALTER TABLE ONLY videos
    ADD CONSTRAINT videos_pkey PRIMARY KEY (id);

ALTER TABLE ONLY video_files
    ADD CONSTRAINT video_files_video_id_fkey FOREIGN KEY (video_id) REFERENCES videos(id);

ALTER TABLE ONLY video_files
    ADD CONSTRAINT video_files_video_library_id_fkey FOREIGN KEY (video_lirary_id) REFERENCES video_libraries(id);

ALTER TABLE ONLY video_files
    ADD CONSTRAINT video_files_storage_id_unique UNIQUE (storage_id);
