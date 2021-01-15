query_text_start_dict <- function() {
  list(
    browse = "
          SELECT 
            image.rowid AS image_id, 
            user.name AS owner,
            painter.name AS painter,
            image.date,
            image.location,
            image.school,
            image.url,
            image.path,
            image.title,
            offered_images.price,
            CASE WHEN offered_images.price NOT NULL THEN 1 ELSE 0 END AS is_offered
          FROM user_image
            INNER JOIN user 
              ON user_image.user_id = user.rowid
            INNER JOIN image
              ON user_image.image_id = image.rowid
            INNER JOIN painter
              ON image.painter_id = painter.painter_id
            LEFT JOIN offered_images
              ON user_image.image_id = offered_images.image_id
        ",
    collection = "
          SELECT 
            image.rowid AS image_id,
            painter.name AS painter,
            image.date,
            image.location,
            image.school,
            image.url,
            image.path,
            image.title,
            offered_images.price,
            CASE WHEN offered_images.price NOT NULL THEN 1 ELSE 0 END AS is_offered
          FROM user_image
            INNER JOIN image
              ON user_image.image_id = image.rowid
            INNER JOIN painter
              ON image.painter_id = painter.painter_id
            LEFT JOIN offered_images
              ON user_image.image_id = offered_images.image_id
        ",
    buy = "
          SELECT 
            image.rowid AS image_id,
            user.name AS owner,
            painter.name AS painter,
            image.date,
            image.location,
            image.school,
            image.url,
            image.path,
            image.title,
            offered_images.price,
            1 AS is_offered
          FROM user_image
            INNER JOIN user
              ON user_image.user_id = user.rowid
            INNER JOIN image
              ON user_image.image_id = image.rowid
            INNER JOIN painter
              ON image.painter_id = painter.painter_id
            INNER JOIN offered_images
              ON user_image.image_id = offered_images.image_id
        "
  )
}