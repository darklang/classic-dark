/*
This migration is in preparation for winding down Darklang-Classic
  context: https://blog.darklang.com/winding-down-darklang-classic/

During the brownout:
- accounts with `keep_active: false` will not be able to log in, or operate generally
- canvases with 'keep_active: false' will be ignored by BwdServer, CronChecker, QueueWorker, etc.
  , effectively disabling the platform for them

After the brownout is complete,
- we will do a one-time backup to all data, in case anyone totally missed the brownout period
- we will delete all accounts and canvases that do not have the keep_active flag on, along with any related data
*/

ALTER TABLE canvases ADD COLUMN keep_active BOOLEAN DEFAULT FALSE;
ALTER TABLE accounts ADD COLUMN keep_active BOOLEAN DEFAULT FALSE
