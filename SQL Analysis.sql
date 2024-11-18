ALTER TABLE `schema`.`road c` 
ADD PRIMARY KEY (`Road_ID`);

ALTER TABLE `schema`.`status c` 
ADD PRIMARY KEY (`Status_ID`);

ALTER TABLE accdeint_c
ADD CONSTRAINT `Status_fk`
  FOREIGN KEY (`Status_ID`)
  REFERENCES `status c` (`Status_ID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION,
  
ADD CONSTRAINT `Road_fk`
  FOREIGN KEY (`Road_ID`)
  REFERENCES `schema`.`road c` (`Road_ID`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;

-- The Number of accidents
SELECT COUNT(*) FROM accdeint_c;

-- which year has the highest number of accidents
SELECT YEAR(ACCIDENT_DATE) AS accident_year, COUNT(*) AS accident_count
FROM accdeint_c
GROUP BY accident_year
ORDER BY accident_count DESC ;

-- The number of accidents at each day
SELECT DAY_WEEK_DESC, COUNT(*) AS accident_count
FROM accdeint_c
GROUP BY DAY_WEEK_DESC
ORDER BY accident_count DESC ;

-- The number of accidents at each period for day
SELECT Day_time, COUNT(*) AS accident_count
FROM accdeint_c
GROUP BY Day_time
ORDER BY accident_count DESC ;

-- The number of accidents at each speed
SELECT SPEED_ZONE, COUNT(*) AS accident_count
FROM accdeint_c
GROUP BY SPEED_ZONE
ORDER BY accident_count DESC ;

-- The number of accidents at each severity type
SELECT SEVERITY, COUNT(*) AS accident_count
FROM accdeint_c
GROUP BY SEVERITY
ORDER BY accident_count DESC ;


-- The number of accidents at each light condition
SELECT light_condition_desc, COUNT(*) AS accident_count
FROM accdeint_c
GROUP BY light_condition_desc
ORDER BY accident_count DESC ;

--  which Accident_Type has the highest number of accidents
SELECT ACCIDENT_TYPE_DESC, COUNT(*) AS accident_count
FROM `status c`
GROUP BY ACCIDENT_TYPE_DESC
ORDER BY accident_count DESC ;

-- which Road geometry has the highest number of accidents
SELECT ROAD_GEOMETRY_DESC, COUNT(*) AS accident_count
FROM `road c`
GROUP BY ROAD_GEOMETRY_DESC
ORDER BY accident_count DESC ;

-- which Road geometry has the highest number of accidents
SELECT RMA, COUNT(*) AS accident_count
FROM `road c`
GROUP BY RMA
ORDER BY accident_count DESC ;

-- If The Number Of Accident Decrease after the rader is avalible or not
SELECT RMA, 
    COUNT(CASE WHEN Sensors = 'yes' THEN 1 END) AS sensor_yes_count,
    COUNT(CASE WHEN Sensors = 'no' THEN 1 END) AS sensor_no_count
FROM `road c`
GROUP BY RMA;

-- show what is the severity of the accident for each road
SELECT rc.RMA, 
    COUNT(CASE WHEN ac.SEVERITY = 'Minor injury' THEN 1 END) AS Minor_injury,
    COUNT(CASE WHEN ac.SEVERITY = 'Moderate' THEN 1 END) AS Moderate,
    COUNT(CASE WHEN ac.SEVERITY = 'Severe injury' THEN 1 END) AS Severe_injury,
    COUNT(CASE WHEN ac.SEVERITY = 'Fatal' THEN 1 END) AS Fatal
FROM `road c` rc 
JOIN accdeint_c ac  
    ON rc.road_id = ac.road_id  
GROUP BY 
    rc.RMA;

-- show what is the severity of the accident for each speed
SELECT SPEED_ZONE ,
	COUNT(CASE WHEN SEVERITY = 'Minor injury' THEN 1 END) AS Minor_injury,
    COUNT(CASE WHEN SEVERITY = 'Moderate' THEN 1 END) AS Moderate,
    COUNT(CASE WHEN SEVERITY = 'Severe injury' THEN 1 END) AS Severe_injury,
    COUNT(CASE WHEN SEVERITY = 'Fatal' THEN 1 END) AS Fatal
FROM 
    accdeint_c
GROUP BY 
    SPEED_ZONE;
    
    
-- show in speed 60 why is the highest number
SELECT 	r.ROAD_GEOMETRY_DESC, 
		r.RMA, 
		s.ACCIDENT_TYPE_DESC, 
		a.SEVERITY, 
		a.light_condition_desc, 
		COUNT(*) as count
FROM `road c` r
JOIN accdeint_c a ON r.Road_ID = a.Road_ID
JOIN `status c` s ON a.Status_ID = s.Status_ID
WHERE a.SPEED_ZONE = 60
GROUP BY r.ROAD_GEOMETRY_DESC, 
		r.RMA, 
		s.ACCIDENT_TYPE_DESC, 
		a.SEVERITY, 
		a.light_condition_desc
ORDER BY count DESC ;
    
-- show why Accident_Type Collision with vehicle is the highest number
SELECT 	r.ROAD_GEOMETRY_DESC, 
		r.RMA, 
        a.SPEED_ZONE,
		a.SEVERITY, 
		a.light_condition_desc, 
		COUNT(*) as count
FROM `road c` r
JOIN accdeint_c a ON r.Road_ID = a.Road_ID
JOIN `status c` s ON a.Status_ID = s.Status_ID
WHERE s.ACCIDENT_TYPE_DESC = 'Collision with vehicle'
GROUP BY r.ROAD_GEOMETRY_DESC, 
		r.RMA, 
		 a.SPEED_ZONE, 
		a.SEVERITY, 
		a.light_condition_desc
ORDER BY count DESC ;

-- Before 2016, a sensor was not available.
SELECT 
    CASE 
        WHEN a.accident_date between '2012-01-01' and '2016-01-01'THEN 'from 2012 to 2016'
        WHEN a.accident_date between '2016-01-01' and '2020-01-01'THEN 'from 2016 to 2020'
    END AS time_period,
    COUNT(*) AS accident_count
FROM 
    accdeint_c a
GROUP BY 
    time_period;

-- 
SELECT 	r.ROAD_GEOMETRY_DESC, 
		r.RMA, 
        a.SPEED_ZONE,
		a.SEVERITY, 
		a.light_condition_desc, 
		COUNT(*) as count
FROM `road c` r
JOIN accdeint_c a ON r.Road_ID = a.Road_ID
JOIN `status c` s ON a.Status_ID = s.Status_ID
WHERE a.light_condition_desc = 'Daylight AM'
GROUP BY r.ROAD_GEOMETRY_DESC, 
		r.RMA, 
		a.SPEED_ZONE, 
		a.SEVERITY, 
		s.ACCIDENT_TYPE_DESC
ORDER BY count DESC ;


