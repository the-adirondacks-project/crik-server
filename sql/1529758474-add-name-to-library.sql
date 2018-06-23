ALTER TABLE libraries 
  ADD COLUMN name text; 
UPDATE libraries SET name = id;
ALTER TABLE libraries
  ALTER COLUMN name SET NOT NULL;
ALTER TABLE libraries 
  ADD CONSTRAINT libraries_name_unique UNIQUE (name);
