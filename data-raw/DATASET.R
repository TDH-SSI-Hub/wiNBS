library(usethis)
library(RODBC)

# county to region
prod<-odbcConnect('NBS_Prod')
page_metadata<-sqlQuery(prod,"SELECT [investigation_form_cd]
      ,[question_identifier]
	  ,[question_label]
      ,[question_tool_tip]
	  ,[required_ind]
	  ,[data_type]
	  ,r.component_type
	  ,c.type_cd_desc
	  ,c.component_behavior
	  ,[nbs_ui_metadata_uid]
      ,m.[nbs_ui_component_uid]
      ,[nbs_question_uid]
      ,[parent_uid]
      ,[display_ind]
      ,[enable_ind]
      ,[max_length]
      ,[order_nbr]
      ,[record_status_cd]
      ,[tab_order_id]
      ,[tab_name]
      ,[future_date_ind_cd]
	  ,[data_location]
      ,[data_use_cd]
      ,[legacy_data_location]
      ,[part_type_cd]
      ,[question_group_seq_nbr]
	  ,c.display_order
  FROM [nbs_odse].[dbo].[NBS_ui_metadata] m
  left join [nbs_odse].[dbo].[NBS_metadata_rule] r
  on m.nbs_ui_component_uid = r.component_uid
  left join [nbs_odse].[dbo].NBS_ui_component c
  on m.nbs_ui_component_uid = c.nbs_ui_component_uid
  where question_identifier is not null and question_identifier <> '------'")
use_data(page_metadata, overwrite = T)

condition_metadata<-sqlQuery(prod,"select * from [nbs_srte].[dbo].[Condition_code]")
use_data(condition_metadata, overwrite = T)
