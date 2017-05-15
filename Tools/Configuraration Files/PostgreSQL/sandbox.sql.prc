SELECT name, context , unit ,
setting, boot_val, reset_val
FROM pg_settings
WHERE name IN ( 'listen_addresses', 'max_connections', 'shared_buffers', 'effec
tive_cache_size', 'work_mem', 'maintenance_work_mem'
)
ORDER BY context, name;