ALTER TABLE IF EXISTS video_files RENAME TO files;
ALTER SEQUENCE IF EXISTS video_files_id_seq RENAME TO files_id_seq;

ALTER TABLE IF EXISTS files RENAME CONSTRAINT video_files_pkey TO files_pkey;
ALTER TABLE IF EXISTS files 
  RENAME CONSTRAINT video_files_storage_id_unique TO files_storage_id_unique;
ALTER TABLE IF EXISTS files RENAME CONSTRAINT video_files_video_id_fkey TO files_video_id_fkey;
ALTER TABLE IF EXISTS files 
  RENAME CONSTRAINT video_files_video_library_id_fkey TO files_library_id_fkey;

ALTER TABLE IF EXISTS files RENAME COLUMN video_library_id TO library_id;


ALTER TABLE IF EXISTS video_libraries RENAME TO libraries;
ALTER SEQUENCE IF EXISTS video_libraries_id_seq RENAME TO libraries_id_seq;

ALTER TABLE IF EXISTS libraries RENAME CONSTRAINT video_libraries_pkey TO libraries_pkey;
